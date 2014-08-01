// Declares State class and model hyperparameters

#ifndef STATE_HPP
#define STATE_HPP

#include <eigen3/Eigen/Dense>
#include <gsl/gsl_randist.h>

///////////////////////////
// Model hyperparameters //
///////////////////////////

// Vague gamma prior on CRP concentration
#define VG_SHAPE 1.0
#define VG_RATE 0.05
#define VG_INIT 1.0

// Normal Gamma prior on cluster-level action parameters
#define NG_SHAPE 1.0
#define NG_RATE 0.05
#define NG_NU 0.5
#define NG_MU 0.0

///////////////////////
// Class declaration //
///////////////////////

class State
{

public:
    // Random number generator
    const gsl_rng* rng;

    // Model variables               // Shape
    double conc; // DP concentration
    Eigen::ArrayXi  z;               // (NUM_OBJECTS)
    Eigen::ArrayXXi events_pos;      // (NUM_ACTIONS,NUM_OBJECTS)
    Eigen::ArrayXXi events_neg;      // (NUM_ACTIONS,NUM_OBJECTS)
    Eigen::ArrayXXd strengths;       // (NUM_ACTIONS,NUM_OBJECTS)
    Eigen::ArrayXi  counts;          // (NUM_CLUSTERS)
    Eigen::ArrayXXd action_means;    // (NUM_ACTIONS,NUM_CLUSTERS)
    Eigen::ArrayXXd action_precs;    // (NUM_ACTIONS,NUM_CLUSTERS)

    // Constructor
    State(const gsl_rng *r, 
          Eigen::Ref<Eigen::ArrayXXi> events_pos, 
          Eigen::Ref<Eigen::ArrayXXi> events_neg);
    
    // Resample the state
    void update(int num_proposals); 

    // For debugging
    void print_vars(bool show_events = false);

private:
    // Internal update functions
    void update_conc();
    void update_z(int num_proposals);
    void update_action_params();
    void update_strengths(int num_proposals);
};

#endif // End include guard
