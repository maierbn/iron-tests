function [abaPath,cmissPath,elemSize,dispOfInterest] = case_selector(LOADING,REFINEMENT,DIMENSION);

switch DIMENSION
    case '3D'
        switch LOADING
            case 'SHEAR'
                dispOfInterest = 1; %x
                switch REFINEMENT
                    case 'COARSE'
                        abaPath = 'current_run\3D_SHEAR_ELASTIC_elem_20_160x120x120mm_intp_1_DIRECT.txt';
                        cmissPath = 'current_run\l160x120x120_n08x06x06_i1_s0.txt';
                        elemSize = 20;
                    case 'MEDIUM'
                        abaPath = 'current_run\3D_SHEAR_ELASTIC_elem_10_160x120x120mm_intp_1_DIRECT.txt';
                        cmissPath = 'current_run\l160x120x120_n16x12x12_i1_s0.txt';
                        elemSize = 10;
                    case 'FINE'
                        abaPath = 'current_run\3D_SHEAR_ELASTIC_elem_5_160x120x120mm_intp_1_DIRECT.txt';
                        cmissPath = 'current_run\l160x120x120_n32x24x24_i1_s0.txt';
                        elemSize = 5;
                end
            case 'UNIAX'
                dispOfInterest = 2; %x
                switch REFINEMENT
                    case 'COARSE'
                        abaPath = 'current_run\.txt';
                        cmissPath = 'current_run\.txt';
                        elemSize = 20;
                    case 'MEDIUM'
                        abaPath = 'current_run\.txt';
                        cmissPath = 'current_run\.txt';
                        elemSize = 10;
                    case 'FINE'
                        abaPath = 'current_run\.txt';
                        cmissPath = 'current_run\.txt';
                        elemSize = 5;                        
                end
        end
    case '2D'
        switch LOADING
            case 'SHEAR'
                dispOfInterest = 1; %x
                switch REFINEMENT
                    case 'COARSE'
                        abaPath = 'current_run\2D_SHEAR_ELASTIC_elemSize_20_160x120mm_intp_1_DIRECT.txt';
                        cmissPath = 'current_run\l160x120x000_n08x06x00_i1_s0.txt';
                        elemSize = 20;
                    case 'MEDIUM'
                        abaPath = 'current_run\2D_SHEAR_ELASTIC_elemSize_10_160x120mm_intp_1_DIRECT.txt';
                        cmissPath = 'current_run\l160x120x000_n16x12x00_i1_s0.txt';
                        elemSize = 10;
                    case 'FINE'
                        abaPath = 'current_run\2D_SHEAR_ELASTIC_elemSize_5_160x120mm_intp_1_DIRECT.txt';
                        cmissPath = 'current_run\l160x120x000_n32x24x00_i1_s0.txt';
                        elemSize = 5;
                end
            case 'UNIAX'
                dispOfInterest = 2; %x
                switch REFINEMENT
                    case 'COARSE'
                        abaPath = 'current_run\.txt';
                        cmissPath = 'current_run\.txt';
                        elemSize = 20;
                    case 'MEDIUM'
                        abaPath = 'current_run\.txt';
                        cmissPath = 'current_run\.txt';
                        elemSize = 10;
                    case 'FINE'
                        abaPath = 'current_run\.txt';
                        cmissPath = 'current_run\.txt';
                        elemSize = 5;
                end                
        end        
end