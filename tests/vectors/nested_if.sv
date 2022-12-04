module top;

logic a, b, c;

always_comb begin
    if (a)
        if (b)
          c = 0;
        else
          c = 1;
    else begin
        c = 0;
    end
end

endmodule