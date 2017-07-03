%
% COMPARE NODAL FORCE VALUES BETWEEN ABAQUS AND OPENCMISS 
%--------------------------------------------------------------------------
% - Uses the free edge for 2D (y = height)
% - Uses the free edge for 3D (y = height & z = width)
%
% - For SHEAR - x displacement is checked
% - For UNIAX - y displacement is checked
%
% - ASSUMES: the correct output vector is availble in the openCMISS file
%       - check "get_displacements.pl" 
%
% 03.07.17
% Harnoor Saini
%
disp ' '
outString = ['COMPARE NODAL FORCE VALUES BETWEEN ABAQUS AND OPENCMISS ...'];
disp(outString)
disp ' '

clear
hold on

%% FILE DEFINITIONS
% (x) working
LOADING = 'SHEAR'; % SHEAR (x); UNIAX
REFINEMENT = 'FINE'; % COARSE (x); MEDIUM (x); FINE
DIMENSION = '2D'; % 2D; 3D (x)

outString = ['Loading is: ' LOADING ', Refinement is: ' REFINEMENT ...
    ', Dimension is: ' DIMENSION];
disp(outString)
disp ' '

[abaPath,cmissPath,elemSize,dispOfInterest] = ...
    case_selector(LOADING,REFINEMENT,DIMENSION);

%% MESH & GEOMETRY DEFINITIONS
% geometry
length = 160;
height = 120;
if DIMENSION == '2D'
    width = 0;
else
    width = 120;
end

% mesh dimensions
numElem_x = round(length/elemSize);
numElem_y = round(height/elemSize);
numElem_z = round(width/elemSize);

% interpolation order
interPol = 1;

% number of nodes
numNodes_x = numElem_x*interPol+1;
numNodes_y = numElem_y*interPol+1;
numNodes_z = numElem_z*interPol+1;

% regular spacing
delta_x = length/numNodes_x;
delta_y = height/numNodes_y;
delta_z = width/numNodes_z;
% create length vectors for plotting
x_vec = 1:delta_x:numNodes_x*delta_x;
y_vec = 1:delta_y:numNodes_y*delta_x;
z_vec = 1:delta_z:numNodes_z*delta_x;

%% FILES READ IN
% read in results files
if numElem_z == 0
    col = [1 2 3];
    formatSpec = '%14s%14s%s%[^\n\r]';
else
    col = [1 2 3 4];
    formatSpec = '%14s%14s%14s%s%[^\n\r]';
end
abaq_field_report = import_field_report(abaPath,col,formatSpec);
cmiss_field_report = import_cmiss_report(cmissPath);

% extract orientation information
% skip over all nodes
nodes = 1;
lnum = 13;
lskip = 0;

% read in nodes (abaqus field report)
while nodes
    if isnan(abaq_field_report{lnum,1})
        % skip header
        lnum = lnum + 1;
    else
        nNumber = abaq_field_report{lnum,1};
        nodal_values(nNumber,1) = nNumber;
        nodal_values(nNumber,2) = abaq_field_report{lnum,2};
        nodal_values(nNumber,3) = abaq_field_report{lnum,3};
        if numElem_z > 0 
            nodal_values(nNumber,4) = abaq_field_report{lnum,4};
        end    
        lnum = lnum + 1;        
        if isnan(abaq_field_report{lnum,1})
            % end of nodes
            nodes = 0;
        end
    end
end

%% PERFORM THE ACTUAL READING IN OF THE NODES OF INTEREST
% total number of nodes
totNodes = (numElem_x*interPol+1)*(numElem_y*interPol+1)*...
    (numElem_z*interPol+1);

if numElem_z == 0
    % top edge (y = H) - 2D
    nSkip = 1;
    topSurf_end = totNodes;
    topSurf_start = totNodes - numElem_x*interPol;
else
    % top edge (y = H & z = W) - 3D
    topSurf_start = 1;
    topSurf_end = topSurf_start;
    nSkip = (numElem_y+1)*(numElem_z+1);
    for i =1:numElem_x
        topSurf_end = topSurf_end + nSkip;
    end
end

k = 1;
for i = topSurf_start:nSkip:topSurf_end
    abaq_topSurf_values(k,1) = nodal_values(i,2);
	abaq_topSurf_values(k,2) = nodal_values(i,3);
    if numElem_z > 0 
        abaq_topSurf_values(k,3) = nodal_values(i,4);
        %in the 3D case the top edge node number between Abaqus and
        %openCMISS is NOT identical (*)
    else
        % in the 2D case the top edge node numbering between Abaqus and
        % openCMISS is identical
        cmiss_topSurf_values(k,1) = cmiss_field_report(i,1);
    end
    k = k+1;
end

outString = ['Start node for ABAQUS is: ' num2str(topSurf_start)];
disp(outString)
outString = ['End node for ABAQUS is: ' num2str(topSurf_end)];
disp(outString)
outString = ['Increment for ABAQUS is: ' num2str(nSkip)];
disp(outString)
disp ' '

%(*)
topSurf_end_cmiss = totNodes;
topSurf_start_cmiss = totNodes - numElem_x;
nSkip = 1;
k = 1;
if numElem_z > 0 
    for i = topSurf_start_cmiss:nSkip:topSurf_end_cmiss
        cmiss_topSurf_values(k,1) = cmiss_field_report(i,1);
        k = k+1;
    end
end

outString = ['Start node for openCMISS is: ' num2str(topSurf_start_cmiss)];
disp(outString)
outString = ['Start node for openCMISS is: ' num2str(topSurf_end_cmiss)];
disp(outString)
outString = ['Increment for openCMISS is: ' num2str(nSkip)];
disp(outString)
disp ' '

% find the l2-norm
a = abaq_topSurf_values(:,dispOfInterest);
b = cmiss_topSurf_values;
l2_norm = sum( sqrt((a-b).^2) );

disp '***************************************************************'
outString = ['SUM OF SQUARE DIFFERENCES IS (L2 = SUM(SQRT{(Xi-Yi)^2}): '...
    num2str(l2_norm)];
disp(outString)
disp '***************************************************************'

%% PLOTTING
figure(1)
title('U1 over top surface')
plot(x_vec,abaq_topSurf_values(:,dispOfInterest),'-r')
plot(x_vec,cmiss_topSurf_values)

% bottom surface (y = 0)
% ...
% left surface (x = 0)
% ...
% right surface (x = L)
% ...