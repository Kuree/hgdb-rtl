module top;

logic a, b, c;

always_comb begin
    case (a)
        0: begin
            case (b)
                0: c = 0;
                1: c = 1;
            endcase
        end
        default:
            c = 0;
    endcase
end

endmodule