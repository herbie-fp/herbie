// Sampling code

#include <ctime>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <eigen3/Eigen/Dense>
#include "state.hpp"

#define USAGE "./infer directory [samples [burn [thin [proposals]]]]"

// Default params
#define SAMPLES 24000
#define BURN 6000
#define THIN 6
#define PROPOSALS 6

using namespace Eigen;
using namespace std;

///////////////////////
// Sampling function //
///////////////////////

// Given the specified sampling parameters, performs MCMC updates and
// computes the posterior mean of object coocurrence/strengths
void infer(State &state, int samples, int burn, int thin, int proposals,
            ArrayXXd &avg_cooc, ArrayXXd &avg_strengths)
{
    int N = state.strengths.cols();
    int A = state.strengths.rows();
    avg_cooc = MatrixXd::Identity(N,N); // Only matrix class has Identity
    avg_strengths = ArrayXXd::Zero(A,N);

    // Burn-in
    for (int i=0;i<burn;i++){
        state.update(proposals);
    }

    // Thinned samples from posterior
    for (int i=0;i<samples-burn;i++){
        state.update(proposals);
        if (i % thin == 0){
            // Update avg_cooc
            for (int j=0;j<N;j++) {
            for (int k=j+1;k<N;k++) {
                bool same = state.z(j) == state.z(k);
                avg_cooc(k,j) += (same - avg_cooc(k,j))/(i+1);
            }
            }
            
            // Update avg_strengths
            ArrayXXd diff = state.strengths - avg_strengths;
            avg_strengths += diff/(i+1);
        }
    }

    // Fill in other side of cooc
    for (int j=0;j<N;j++) {
    for (int k=j+1;k<N;k++) {
        avg_cooc(j,k) = avg_cooc(k,j);
    }
    }
}

//////////////////
// IO Functions //
//////////////////

void events_from_file(string file, ArrayXXi &pos, ArrayXXi &neg)
{
    // First we compute the size of the file (we assume correct formatting)
    ifstream in(file.c_str());
    string line,field;
    getline(in,line);

    // Get the first line to determine the number of cols
    int cols = 0;
    stringstream firstrow(line);
    while (getline(firstrow,field,',')){
        cols++;
    }

    // Get the rest of the lines to determine the number of rows
    int rows = 1;
    while (getline(in,line)){
        rows++;
    }
    in.close();

    // Now we rescan the file, and split the data into pos/neg event counts
    // (Note that data is stored tranposed relative to CSV layout).
    in.open(file.c_str());
    pos.resize(cols/2,rows);
    neg.resize(cols/2,rows);
    for (int r=0;r<rows;r++){
        getline(in,line);
        stringstream stream(line);
        bool even = true;

        for (int c=0;c<cols;c++){
            getline(stream,field,',');
            if (even){
                pos(c/2,r) = atoi(field.c_str());
            } else {
                neg(c/2,r) = atoi(field.c_str());
            }
            even = !even;
        }
    }
    in.close();
}

// Writes an coccurence array to standard error
void array_to_file(string file, const Ref<const ArrayXXd> &arr)
{
    ofstream out(file.c_str());
    int C = arr.cols();
    int R = arr.rows();

    for (int c=0;c<C;c++){
        for (int r=0;r<R;r++){
            out << arr(r,c);
            if ( R-r > 1 ){
                out << ',';
            }
        }
        out << endl;
    }
}

///////////////////
// Main function //
///////////////////

int main(int argc, char *argv[])
{
    // Parse the options
    if (argc < 2 || argc > 6) {
        cout << USAGE << endl;
        return 1;
    }

    // Sampling parameters
    int samples = SAMPLES;
    int burn = BURN;
    int thin = THIN;
    int proposals = PROPOSALS;
    if (argc > 2){
        samples = atoi(argv[2]);
    if (argc > 3){
        burn = atoi(argv[3]);
    if (argc > 4){
        thin = atoi(argv[4]);
    if (argc > 5){
        proposals = atoi(argv[5]);
    }}}}

    // Directory location
    string dir = argv[1];
    if (dir[dir.length()] != '/'){ // Kind of a hack...
        dir += '/';
    }
    string events_file = dir + "events.csv";
    
    // Import the data
    ArrayXXi pos,neg;
    events_from_file(events_file,pos,neg);

    // Create the particle
    gsl_rng *rng = gsl_rng_alloc(gsl_rng_default);
    State particle(rng,pos,neg);

    // Sample and compute the posterior co-occurence/strengths
    ArrayXXd avg_cooc,avg_strengths;
    clock_t begin = clock();
    infer(particle,samples,burn,thin,proposals,avg_cooc,avg_strengths);
    double duration = float(clock() - begin) / CLOCKS_PER_SEC;
    cout << "Took : " << duration << " seconds" << endl;
    cout << "Final state: " << endl;
    particle.print_vars(true);

    // Write the posterior averages to disk
    string cooc_file = dir + "cooc.csv";
    array_to_file(cooc_file,avg_cooc);

    string strengths_file = dir + "strengths.csv";
    array_to_file(strengths_file,avg_strengths);

    gsl_rng_free(rng);
    return 0;
}
