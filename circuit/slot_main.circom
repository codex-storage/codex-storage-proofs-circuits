pragma circom 2.0.0;
include "sample_cells.circom";
component main {public [entropy,slotRoot]} = SampleAndProveV1(1024, 9, 20);
