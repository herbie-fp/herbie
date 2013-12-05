function simulate3D()
    global PAR
    PAR = param3D();
    %test
    initializePoisson();
%     
%     %start at high temperature, no gate, no field
%     PAR.KbT = 120*PAR.Kb;
%     PAR.FreezeImp = 0;
%     PAR.Gates = 0;
%     PAR.magField = 0;
%     PAR.LongIter = 0;
%     initialIter() %run Poisson a few times to set up from impurities
%     iterate() %iterate with no field, under heavy approximation
%     
%     %low temperature, gate and magnetic field turned on, 
%     %impurities frozen in
%     PAR.KbT = 1*PAR.Kb;
%     PAR.FreezeImp = 1;
%     PAR.Gates = 1; 
%     PAR.magField = 1;
%     iterate(); %iterate with the top gate and field under approximation
%     
%     %now get the spectrum by iterating on the full Hamiltonian
%     PAR.LongIter = 1;
%     iterate();
%     
%     plotResults()

end

function initialIter()
    global PAR
    initializePoisson() %initialize Poisson boundary conditions and impurity density
    initialCount = 0;
    
    while min(PAR.Veff(:)) > 0.1 && initialCount < 50
        poisson();
        initialCount = initialCount + 1;
    end
end

function iterate()
    global PAR
    count = 0;
    while PAR.Delta > PAR.DeltaStop && count < PAR.countStop
        if PAR.LongIterate
            fprintf('Hard iteration starting.'
            longSchrodinger(PAR.carrier);
        else
            simpleSchrodinger(PAR.carrier);
        end
        poisson();
        count = count + 1;
        if PAR.verbose
            diagnosticPlots();
        end
    end
    
    if count == PAR.countStop
        fprintf('Max iterations (%i) reached.', PAR.countStop)
    else
        fprintf('Iterations needed at T = %i: %i\n', PAR.KbT/PAR.Kb, count)
    end
end

%global convention: vectorize the lattice as (x,y,z) = (1,1,1),(2,1,1),...
%(nx,1,1),(1,2,1)...
%PAR.nextZ = nx*ny is the number of points at fixed z

function initializePoisson()
    global PAR
    
    if (PAR.FreezeImp == 0)
        %Fermi-Dirac statistics for charge on impurities
        PAR.NdNa = PAR.NdChar./(1+exp(-(PAR.Ec-PAR.Vef-PAR.EDonor -PAR.mu)/PAR.KbT))...
               + PAR.NaChar./(1+exp( (PAR.Ev+PAR.Vef+PAR.EAccept-PAR.mu)/PAR.KbT));
    end
    
    poissonSize = PAR.meshSize - 2*PAR.nextZ; %leave out faces specified by Dirichlet
    PAR.boundaryVector = sparse(poissonSize,1);
    %Dirichlet conditions at the top and bottom
    topGate = zeros(PAR.nx, PAR.ny);
    topGate(PAR.iTopGateXStart:PAR.iTopGateXStop, PAR.iTopGateYStart:PAR.iTopGateYStop) = PAR.TopGate;
    topGate = topGate(:);
    bottomGate = zeros(PAR.nx, PAR.ny);
    bottomGate(PAR.iBottomGateXStart:PAR.iBottomGateXStop, PAR.iBottomGateYStart:PAR.iBottomGateYStop) = PAR.BottomGate;
    bottomGate = bottomGate(:);
    PAR.boundaryVector(1:PAR.nextZ) = topGate;
    PAR.boundaryVector(end-PAR.nextZ:end) = bottomGate;
    
    %Neumann conditions on the sides
    PAR.boundaryVector = PAR.boundaryVector + (PAR.isXEdge+PAR.isYEdge).*...
        PAR.density(PAR.nextZ+1:end-PAR.nextZ)./(2*PAR.epsilon(PAR.nextZ+1:end-PAR.nextZ));
    
    %build the Laplacian matrix.
    %Anything on an x or y edge only connects to one point in the edge
    %direction - this adjusts the diagonal entry. Also we need to skip off
    %diagonal entries at edges.
    xDiag = (-2*(~PAR.isXEdge) - PAR.isXEdge)/(PAR.dx)^2;
    yDiag = (-2*(~PAR.isYEdge) - PAR.isYEdge)/(PAR.dy)^2;    
    diagonal = xDiag + yDiag - 2/(PAR.dz)^2;
    offDiagX = (mod(1:poissonSize, PAR.nx) ~= 1).';
    offDiagY = repmat([0 ones(1,PAR.ny-1)],PAR.nx,PAR.nz-2);
    offDiagY = offDiagY(:);
    offDiagZ = ones(poissonSize,1);
    %7-diagonal Laplacian matrix
    PAR.Laplacian = spdiags([diagonal offDiagZ offDiagZ [offDiagX(2:end);1] offDiagX ...
        offDiagY [offDiagY(PAR.nx+1:end); ones(PAR.nx,1)]], [0, PAR.nextZ, -PAR.nextZ, -1, 1, PAR.nx, ...
        -PAR.nx], poissonSize, poissonSize);
end

function poisson()
    %NOTE: I'm ignoring the fact that the dielectric constant has a spatial
    %derivative and just 
    global PAR
    
    %actually do Poisson
    
    PAR.phi(:,2) = PAR.phi(:,1);
end

function simpleSchrodinger(carrier)
    global PAR
    schrIndices = PAR.iSchStart:PAR.nextZ:PAR.iSchStop; %for a given xy point,
    %these are the indices over which I want to solve Schrodinger
    
    if (PAR.FreezeImp == 0)
        %Fermi-Dirac statistics for charge on impurities
        PAR.impurityDensity = PAR.donorCharge./(1+exp(-(PAR.Ec-PAR.Vef-PAR.EDonor -PAR.mu)/PAR.KbT))...
               + PAR.acceptorCharge./(1+exp( (PAR.Ev+PAR.Vef+PAR.EAccept-PAR.mu)/PAR.KbT));
    end;
    
    PAR.Den = PAR.impurityDensity;
    
    nz  = PAR.SchLen; 

    %solve Schrodinger locally at each xy index. Finite differences.
    for xyIndex = 1:PAR.nextZ
        m = carrier.m(schrIndices);
        band = carrier.band(schrIndices);
        ch = carrier.charge(schrIndices);
        m1 = 2./(1./m(schrIndices)+1./m(schrIndices+1));
        m2 = 2./(1./m(schrIndices)+1./m(schrIndices-1));
        H1 = PAR.Ekin*(1./m1+1./m2)/PAR.dz^2; %kinetic part of the diagonal
        H1 = H1+PAR.Vef(schrIndices)+band(schrIndices); %potential part of the diagonal
        H2 = -PAR.Ekin./m1(1:nz-1)./PAR.dz^2; %off-diagonal terms
        H2p = [1, H2]; %fluff required for spdiags to work properly
        Ham= spdiags([H1', H2', H2p'], [0,-1,1],nz,nz);
        
        %diagonalize. Get PAR.nbands eigenvalues closest to 0 energy. May
        %need to change guess from 0 to something else.
        [PAR.Evec,PAR.Eval] = eigs(Ham,PAR.nbands,0);
        PAR.Eval = diag(PAR.Eval);
        
        %now add to density at this location
        if PAR.magField
            llEnergies = ((1:PAR.nLLs)+1/2)*PAR.hbarOmega;
            for i=1:length(PAR.Eval)
                fermi = 1./(exp((PAR.Eval(i)+llEnergies-PAR.mu)/PAR.KbT)+1);
                xyDen = ch*sum(fermi)/PAR.lB^2; %missing something, not exactly sure whatpo
                PAR.Den(schrIndices) = PAR.Den(schrIndices)+xyDen*abs(PAR.Evec(:,i)).^2;
            end
        else
           fermi = 1./(exp(ch*(PAR.Eval-PAR.mu)/PAR.KbT)+1);
           %density is fermi function * DOS for 2D free electrons?? *
           %|wavefunction|^2. Check my units.
           ms = repmat(m,1,PAR.nbands);
           PAR.Den(schrIndices) = PAR.Den(schrIndices)+sum(fermi*ms/2/pi/PAR.Ekin*abs(PAR.Evec).^2,2);
        end
        
        schrIndices = schrIndices + 1; %move to the next xy point
    end
end

function longSchrodinger()
    global PAR
    %build
end

function plotResults()
    global PAR
    %build
end

function diagnosticPlots()
    global PAR
    %build
end