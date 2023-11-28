pragma circom 2.0.0;
include "sample_cells.circom";
// SampleAndProven( maxDepth, maxLog2NSlots, blockTreeDepth, nFieldElemsPerCell, nSamples ) 
component main {public [entropy,dataSetRoot,slotIndex]} = SampleAndProve(16, 5, 3, 5, 10);
