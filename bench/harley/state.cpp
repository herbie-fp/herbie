// Implements a Markov state class for MCMC sampling

#include <iostream>
#include <math.h>
#include "state.hpp"

using namespace Eigen;
using namespace std;

/////////////////////
// Print variables //
/////////////////////
void State::print_vars(bool show_events)
{
    if (show_events){
        cout << "positives: " << endl;
        cout << events_pos << endl;
        cout << "negatives: " << endl;
        cout << events_neg << endl;
    }
    cout << "conc: " << conc << endl;
    cout << "z: " << z.transpose() << endl;
    cout << "counts: " << counts.transpose() << endl;
    cout << "strengths: " << endl;
    cout << strengths << endl;
    cout << "action means: " << endl;
    cout << action_means << endl;
    cout << "action precs: " << endl;
    cout << action_precs << endl;
}

//////////////////////
// Helper functions //
//////////////////////

// Returns index of first zero element, or -1 if none exists
int find_first_zero(const Ref<const ArrayXi> &vec)
{
    for (int i=0;i<vec.size();i++)
        if (vec(i) == 0) {return i;}
    return -1;
}    

// Returns p(pos,neg | new_s) / p(pos,neg | old_s)
double s_lh_ratio(int pos, int neg, double new_s, double old_s)
{
    /* Original, naive implementation
    double r1 = 1 / (1 + exp(-new_s));
    double r2 = 1 / (1 + exp(-old_s));
    return (pow(r1,pos) * pow(r1,neg))/(pow(r2,pos) * pow(r2,neg));
    */
  
    /* Older less efficient/stable version
    double r1 = (1+exp(-old_s))/(1+exp(-new_s));
    double r2 = (1+exp(old_s))/(1+exp(new_s));
    return pow(r1,pos)*pow(r2,neg);
    */

    // More efficient/stable version, 
    double new_t = exp(-new_s);
    double old_t = exp(-old_s);
    double a = pos*log( (1+old_t)/(1+new_t) );
    double r = (1 - 1/(1+new_t)) / (1 - 1/(1+old_t));
    return exp(a + neg*log(r));
}    

// Returns p(cluster | new_params) / p(cluster | old_params)
double z_lh_ratio(const Ref<const ArrayXd> &strengths,
                  const Ref<const ArrayXd> &new_means, 
                  const Ref<const ArrayXd> &new_precs,
                  const Ref<const ArrayXd> &old_means,
                  const Ref<const ArrayXd> &old_precs)
{
    double prec_term  = (new_precs/old_precs).sqrt().prod();
    ArrayXd new_diffs = strengths - new_means;
    ArrayXd new_quads = new_precs*new_diffs*new_diffs;
    ArrayXd old_diffs = strengths - old_means;
    ArrayXd old_quads = old_precs*old_diffs*old_diffs;
    return prec_term * (0.5*(old_quads-new_quads)).exp().prod();
}

// Returns a draw from a normal gamma distribution
void ng_draw(const gsl_rng *r, double &mean, double &prec,
             double shape, double rate, double nu, double mu)
{
    prec = gsl_ran_gamma(r,shape,1/rate);
    mean = mu + gsl_ran_gaussian(r,1/sqrt(nu*prec));
}

// Resamples action parameters from the prior distribution
// Assumes that means and precs are of the same size
void prior_params(const gsl_rng *r, Ref<ArrayXd> means, Ref<ArrayXd> precs)
{
    for (int i=0;i<means.size();i++){
        ng_draw(r,means(i),precs(i),NG_SHAPE,NG_RATE,NG_NU,NG_MU);
    }
}

// Computes the sufficient statistics for the posterior distribution
// on the action parameters for each action for each cluster
void suff_stats(Ref<ArrayXi> ns, Ref<ArrayXXd> ms, Ref<ArrayXXd> ss,
                const Ref<const ArrayXi> &z, 
                const Ref<const ArrayXXd> &strengths)
{
    for (int i=0;i<z.size();i++) {
        int k = z(i);
        ArrayXd diffs = strengths.col(i) - ms.col(k);
        ns(k) += 1;
        ms.col(k) += diffs / ns(k);
        ss.col(k) += diffs * (strengths.col(i) - ms.col(k));
    }
}

// Resamples action parameters from the posterior distribution
// using the sufficient statistics (count,strength means, strength ssds)
void posterior_params(const gsl_rng *r,
                      Ref<ArrayXXd> means, Ref<ArrayXXd> precs,
                      const Ref<const ArrayXi>  &ns,
                      const Ref<const ArrayXXd> &ms,
                      const Ref<const ArrayXXd> &ss)
{
    int K = ns.size();
    int A = ms.rows();

    // Then we draw new strength params for each non-empty cluster
    for (int k=0;k<K;k++){
        if (ns(k) > 0){
            // Compute posterior parameters from sufficient stats
            double nu    = NG_NU + ns(k);
            double shape = NG_SHAPE + ns(k);
            ArrayXd mu   = (NG_NU*NG_MU + ns(k)*ms.col(k))/nu;
            ArrayXd ds   = ms.col(k) - NG_MU;
            ArrayXd rate = NG_RATE + 0.5*(ss.col(k) + NG_NU*ns(k)*ds*ds/nu);

            for (int a=0;a<A;a++){
                ng_draw(r,means(a,k),precs(a,k),shape,rate(a),nu,mu(a));
            }
        }
    }
}

/////////////////
// Constructor //
/////////////////

State::State(const gsl_rng *r, Ref<ArrayXXi> pos, Ref<ArrayXXi> neg):
    rng(r),events_pos(pos),events_neg(neg)
{
    int N = events_pos.cols();
    int A = events_pos.rows();
    
    // Begin with all objects in the same cluster
    z = ArrayXi::Zero(N);
    counts = N*ArrayXi::Ones(1);

    // Initialize the strengths to 1-smoothed MLE estimates
    ArrayXXd pos_d = events_pos.cast<double>();
    ArrayXXd neg_d = events_neg.cast<double>();
    ArrayXXd mle_prob = (pos_d + 1.0) / (pos_d + neg_d + 2.0);
    strengths = mle_prob.log() - (1-mle_prob).log();

    // Draw other variables from their prior distributions
    conc = VG_INIT;
    action_means.resize(A,1); action_precs.resize(A,1);
    prior_params(r,action_means.col(0),action_precs.col(0));
}

//////////////////////
// Update functions //
//////////////////////

void State::update_z(int num_proposals)
{
    int N = z.size();
    int A = strengths.rows();

    // For each object, we do the following 'num_proposals' times:
    // 1) Propose a new assignment using the CRP conditional given by 
    //    the assignments of all other objects
    // 2) Accept the new cluster assignment with probability equal to
    //    p(object | new cluster params) / p(object | old cluster params)
    for (int i=0;i<N;i++)
    {
        ArrayXd strs = strengths.col(i);

        // Remove point from its cluster, keeping track of old params
        int k = z(i);
        counts(k) -= 1;
        ArrayXd means = action_means.col(k);
        ArrayXd precs = action_precs.col(k);
        int K = counts.size(); // Number of instantiated clusters
                               // (Some may be empty)

        // Compute the weight vector/initialize the RNG
        double *weights = new double[K+1];
        for (int j=0;j<K;j++)
            weights[j] = counts(j);
        weights[K] = conc;
        gsl_ran_discrete_t *d = gsl_ran_discrete_preproc(K+1,weights);

        // Propose new assignments and accept/reject with probability
        // equal to the likelihod ratio of the new/old parameters
        ArrayXd new_means(A);
        ArrayXd new_precs(A);
        for (int j=0;j<num_proposals;j++)
        {
            // Draw a new cluster assignment
            int new_k = gsl_ran_discrete(rng,d);

            // Get its action parameters
            if (new_k == K) { // New cluster, draw from prior
                prior_params(rng,new_means,new_precs);
            } else if (new_k != k) { // Old, different cluster
                new_means = action_means.col(new_k);
                new_precs = action_precs.col(new_k);
            } else { // Same cluster, LH ratio is 1.0 so we can skip
                continue;
            }

            // Accept/reject
            double ratio = z_lh_ratio(strs,new_means,new_precs,means,precs);
            double cutoff = gsl_ran_flat(rng,0.0,1.0);
            if (ratio >= cutoff) {
                k = new_k;
                means = new_means;
                precs = new_precs;
            }
        }

        // Add the point to its new cluster
        if (k < K) { // Add to an existing cluster (may be empty)
            z(i) = k;
            counts(k) += 1;
        } else { // New cluster, must fill empty slot or extend state
            int slot = find_first_zero(counts);
            if (slot >= 0) { // Empty slot exists
                z(i) = slot;
                counts(slot) += 1;
                action_means.col(slot) = new_means;
                action_precs.col(slot) = new_precs;
            } else { // No slot, extend state, shouldn't happen often
                z(i) = K;
                counts.conservativeResize(K+1);
                counts(K) = 1;
                action_means.conservativeResize(A,K+1);
                action_means.col(K) = means;
                action_precs.conservativeResize(A,K+1);
                action_precs.col(K) = precs;
            }
        } 

        // Clean up
        delete [] weights;
        gsl_ran_discrete_free(d);
    }
}

void State::update_action_params()
{
    int N = z.size();
    int K = counts.size();
    int A = strengths.rows();

    // Compute the sufficient statistics for each cluster,
    // and draw new parameters from the associated posterior
    ArrayXi  ns = ArrayXi::Zero(N);
    ArrayXXd ms = ArrayXXd::Zero(A,K);
    ArrayXXd ss = ArrayXXd::Zero(A,K);
    suff_stats(ns,ms,ss,z,strengths);
    posterior_params(rng,action_means,action_precs,ns,ms,ss);
}

void State::update_strengths(int num_proposals)
{
    int N = z.size();
    int A = strengths.rows();
    
    for (int i=0;i<N;i++)
    {
        int k = z(i);
        for (int a=0;a<A;a++)
        {
            // Get params
            int pos = events_pos(a,i);
            int neg = events_neg(a,i);
            double s = strengths(a,i);
            double mean = action_means(a,k);
            double sig = 1.0/sqrt(action_precs(a,k));

            // Accept/reject new proposals
            double new_s;
            for (int j=0;j<num_proposals;j++){
                new_s = mean + gsl_ran_gaussian(rng,sig);
                double ratio = s_lh_ratio(pos,neg,new_s,s);
                double cutoff = gsl_ran_flat(rng,0.0,1.0);
                if (ratio >= cutoff)
                    s = new_s;
            }

            // Update with new value
            strengths(a,i) = s;
        }
    }
}

void State::update_conc()
{
    // Compute sufficient statistics
    int N = z.size();
    int K = 0; // Number of non empty clusters
    for (int i=0;i<counts.size();i++)
         K += counts[i] > 0;

    // Draw the auxiliary variable
    double x = gsl_ran_beta(rng,conc+1,N);
    double b = 1/(VG_RATE - log(x));
    double g0 = gsl_ran_gamma(rng,VG_SHAPE+K,b);
    double g1 = gsl_ran_gamma(rng,VG_SHAPE+K-1,b);
    double mix = 1/(1+b*(VG_SHAPE+K-1)/N);
    conc = mix*g0 + (1-mix)*g1;
}

void State::update(int num_proposals)
{
    update_z(num_proposals);
    update_action_params();
    update_strengths(num_proposals);
    update_conc();
}
