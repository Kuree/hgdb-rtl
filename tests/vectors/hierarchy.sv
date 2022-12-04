module child (
    input logic a,
    input logic b;
);

logic c;

endmodule

module top;

logic a;
logic b;

child inst(.*);

endmodule