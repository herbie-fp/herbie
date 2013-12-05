function PAR = param3D()
    %%% Useful constants
    Ryd  = 13.6;   %ev
    Bhor = 0.529;  %A ; Bohr radius
    PAR.Ekin = Ryd*Bhor^2; %eV*A^2 %hbar^2/2/m   Ryd = Hatree energy/2
    PAR.e2Eps0    = 8*pi*Ryd*Bhor; %eV*A     e^2*epsilon0
    PAR.Kb = 8.617e-5; %eV/K kb

    % Constants dependent on AlxGa(1-x)As, x=0.0 - 0.45
    % Poly. depend: 0th    1st    2nd order
    decAlGaAs  = [ 0.000  0.773  0.000]; % eV
    egapAlGaAs = [ 1.424  1.247  0.000]; % eV
    barrAlGaAs = [ 0.600  0.470  0.000]; % eV  %usualy 0th = 0.6
    meAlGaAs   = [ 0.067  0.083  0.000]; % m0
    mhhAlGaAs  = [-0.480 -0.031  0.000]; % m0
    mlhAlGaAs  = [-0.082 -0.0053 0.000]; % m0
    epsiAlGaAs = [13.100 -3.000  0.000]; % eps0
  
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %User inputs
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    PAR.Lx = 1000; %A
    PAR.Ly = 1000; %A
    PAR.dx = 10; %A
    PAR.dy = 10; %A
    PAR.dz = 10; %A
    
    PAR.struct = [
            220  0      0        0         0.03       0.04 %22 nm GaAs
            300  0.3    0        0         0.03       0.04 %30 nm AlGaAs 30%
            10   0.3    10        0         0.03       0.04 %delta doping, 1 nm
            700  0.3    0        0         0.03       0.04 %50 nm AlGaAs 30%
            200  0      0        0         0.03       0.04 %20 nm GaAs layer
            1400 0.02   0        0         0.03       0.04 %140 nm AlGaAs 2%
            200  0      0        0         0.03       0.04 %20 nm GaAs
            700  0.3    0        0         0.03       0.04 %50 nm AlGaAs 30%
            10   0.3    10       0         0.03       0.04 %delta doping, 1 nm
            300  0.3    0        0         0.03       0.04 %30 nm AlGaAs 30%
            220  0      0        0         0.03       0.04 %22 nm GaAs
     ];
    
    %gate information
    PAR.TopGate = 1.3; %V, up to a factor of gate dielectric
    PAR.BottomGate = 2.6; %V, up to a factor of gate dielectric
    PAR.TopGateXStart = 0;
    PAR.TopGateXStop = 1000;
    PAR.TopGateYStart = 0;
    PAR.TopGateYStop = 1000;
    
    %Magnetic field
    PAR.B = 1; %Tesla
    
    %where to solve Schrodinger
    PAR.SchStart = 930;
    PAR.SchStop  = 3330;

    %Carrier choice
    PAR.carrier = PAR.ee;
    
    %chemical potential
    PAR.mu = 0;
    
    %Settings
    PAR.dpstop = 1e-10; % End-condition on size of potential correction
    PAR.verbose   = 0; % Show diagnostic (0 = none, 1 = at end, 2 = during)
    PAR.nbands = 10; % Number of bands taken from Hamiltonian. Careful
                  % making this number lower; might give spurious results
    PAR.iterLim = 1000; % Number of iterations before simulation gives up
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Calculated parameters
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %mesh size information
    PAR.Lz = sum(PAR.struct(:,1));
    PAR.nz = PAR.Lz/PAR.dz+length(PAR.struct(:,1));
    PAR.nx = PAR.Lx/PAR.dx;
    PAR.ny = PAR.Ly/PAR.dy;
    PAR.nextZ = PAR.nx*PAR.ny;
    PAR.meshSize = PAR.nextZ*PAR.nz;
    %which points are on an x or y edge
    PAR.isXEdge = ((mod(1:poissonSize, PAR.nx) == 1) + (mod(1:poissonSize, PAR.nx)==0)).';
    PAR.isYEdge = repmat([1 zeros(1,PAR.ny-2) 1],PAR.nx,PAR.nz-2);
    PAR.isYEdge = PAR.isYEdge(:);

    % Make empty arrays corresponding to the structure grid
    pfields={'xAl','donorCharge','acceptorCharge','EDonor','EAccept','z','Ec','Ev','mhh','me','mlh','epsilon','impurityDensity','dzm','Den',...
           'Denele','Denhh','Denlh','EleFie','Vef','VefOld','scale'};
    for ii=1:length(pfields)
        eval(sprintf('PAR.%s=zeros(%1.0f,1);',pfields{ii},PAR.meshSize));
    end

  
  % From input structure, determine local parameters (spacing, Al %, etc.)

  %grid labeled as (x,z)
  zGridPoints = 0;
  for layerNum=1:size(PAR.struct,1);
      zGridPoints=(max(zGridPoints)+1):(max(zGridPoints)+(PAR.struct(layerNum,1)/PAR.dz+1)*PAR.nextZ);
      PAR.xAl(zGridPoints)     = PAR.struct(layerNum,2);
      PAR.NdChar(zGridPoints)  = PAR.struct(layerNum,3)*1e-6; %convert 10^18 cm^-3 to A^-3
      PAR.NaChar(zGridPoints)  = -PAR.struct(layerNum,4)*1e-6;
      PAR.EDonor(zGridPoints)  = PAR.struct(layerNum,5);
      PAR.EAccept(zGridPoints) = PAR.struct(layerNum,6);
  end
  
  % Calculate more local parameters (bands, effective mass, etc.)
  ii=[3 2 1];
  PAR.me    = polyval(meAlGaAs(ii)  , PAR.xAl);
  PAR.mhh   = polyval(mhhAlGaAs(ii) , PAR.xAl);
  PAR.mlh   = polyval(mlhAlGaAs(ii) , PAR.xAl);
  PAR.epsilon  = polyval(epsiAlGaAs(ii), PAR.xAl);
  %%%%These need fixing
  PAR.barrL = polyval(barrAlGaAs(ii), PAR.xAl(1,:));   % Left barrier E gap
  PAR.barrR = polyval(barrAlGaAs(ii), PAR.xAl(end,:)); % Right barrier E gap
  PAR.barrT = polyval(barrAlGaAs(ii), PAR.xAl(1:nextZ));   % Top barrier E gap
  PAR.barrB = polyval(barrAlGaAs(ii), PAR.xAl(end-nextZ:end); % Bottom barrier E gap
  
  PAR.barrT=PAR.barrB;
  PAR.barrR=PAR.barrR;
  
  % Local conduction and valence band energies
  PAR.Ec    = PAR.mu + polyval(decAlGaAs(ii),PAR.xAl)...
              + repmat(linspace(PAR.barrT(1),PAR.barrB(1),PAR.nz),PAR.nx,1);
  PAR.Ev    = PAR.Ec - polyval(egapAlGaAs(ii),PAR.xAl);
  
  % Find index corresponding to SchStart and Stop depths
  PAR.iSchStart = PAR.SchStart/PAR.dz+1;
  PAR.iSchStop  = PAR.SchStop/PAR.dz+1;
  PAR.SchLen    = PAR.iSchStop-PAR.iSchStart+1;
  
  % Carrier definitions
  PAR.ee=struct('ch',-1,'m',PAR.me ,'band',PAR.Ec);
  PAR.hh=struct('ch', 1,'m',PAR.mhh,'band',PAR.Ev);
  PAR.lh=struct('ch', 1,'m',PAR.mlh,'band',PAR.Ev);
  
end