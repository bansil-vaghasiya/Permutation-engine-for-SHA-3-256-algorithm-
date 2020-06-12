module perm(input clk, input reset, input [2:0] dix,input [199:0] din,
            input pushin, output [2:0] doutix, output [199:0] dout,
            output pushout);
  reg [4:0][4:0][63:0] t_in,t_out,ro_in, ro_out, pi_in, pi_out, si_in, si_out, tou_in, tou_out;
  reg [7:0][199:0] tmp_dat, rnd_out, rnd_nxt, fin_out, dat;
  reg [199:0] dout_d;
  reg [4:0][63:0] c,d;
  reg [63:0] R;
  reg [23:0][63:0] RC;
  reg new_rq;
  reg [2:0] doutix_d;
  reg theta_st, pushout_d, pend_dat;
  reg [3:0] rnd;
  integer i,j,k,r,t,t_1,tc;

  // Assign outputs
  assign doutix=doutix_d;
  assign dout=dout_d;
  assign pushout=pushout_d;

  // Initialize the Round Constants
  assign RC[0]	        = 64'h0000000000000001;	
  assign RC[1]	        = 64'h0000000000008082;
  assign RC[2]	        = 64'h800000000000808A;	
  assign RC[3]	        = 64'h8000000080008000;	
  assign RC[4] 	        = 64'h000000000000808B;	
  assign RC[5]	        = 64'h0000000080000001;	
  assign RC[6]	        = 64'h8000000080008081;	
  assign RC[7]	        = 64'h8000000000008009;	
  assign RC[8]	        = 64'h000000000000008A;	
  assign RC[9]	        = 64'h0000000000000088;	
  assign RC[10]	        = 64'h0000000080008009;	
  assign RC[11]	        = 64'h000000008000000A;	
  assign RC[12]	        = 64'h000000008000808B;
  assign RC[13]	        = 64'h800000000000008B;
  assign RC[14]	        = 64'h8000000000008089;
  assign RC[15]       	= 64'h8000000000008003;
  assign RC[16]	        = 64'h8000000000008002;
  assign RC[17]	        = 64'h8000000000000080;
  assign RC[18] 	= 64'h000000000000800A;
  assign RC[19]	        = 64'h800000008000000A;
  assign RC[20] 	= 64'h8000000080008081;
  assign RC[21]	        = 64'h8000000000008080;
  assign RC[22] 	= 64'h0000000080000001;
  assign RC[23]	        = 64'h8000000080008008;

  // Modulus without the % operator
  // No issues with synthesis
  function integer modulus;
    input integer num,div;
    integer i;
    if(num<0) begin
      for(i=num;i<0;i=i+div);
      modulus = i;
    end else if(num>=0) begin
      for(i=num;i>=div;i=i-div);
      modulus = i;
    end
  endfunction

  always @(posedge(clk) or posedge(reset)) begin
    if(reset) begin
      tmp_dat <= 0;
      theta_st <= 0;
      doutix_d <= 0;
      dout_d <= 0;
      pushout_d <= 0;
      pend_dat <= 0;
      new_rq <= 0;
      dat <= 0;
      fin_out <= 0;
    end else begin
      // Input Stage - Just get inputs and once done handover to next block
      // Takes 8 Cycles - Runs in parallel to other stages 
      if (pushin) begin
        tmp_dat[dix] <= #1 din;
        if (dix == 7) begin
          dat <= #1 tmp_dat;
          dat[dix] <= #1 din;
          theta_st <= #1 1;
          rnd <= #1 0;
        end
      end

      // Permutation Stage - 8 Cycles
      // Take data from input Stage and 
      // pass data on to the comb block
      // Comb block does 3 rounds per cycle
      // Runs in parallel to other stages
      if (theta_st && rnd<7) begin
        pend_dat <= #1 1;
        dat <= #1 rnd_out;
        rnd <= #1 rnd + 1; 
      end

      // Output stage - 8 Cycles
      // Take data from Permutation stage and drive to outputs
      // Runs in parallel to other stages
      if(rnd==7 && pend_dat) begin
        pend_dat <= #1 0;
        fin_out <= #1 rnd_out;
        dout_d <= #1 rnd_out[0];
        pushout_d <= #1 1;
        if (!pushin) begin
          theta_st <= #1 0;
        end
      end
    
      if(pushout_d != 0 && doutix_d != 7) begin
        doutix_d <= #1 (doutix_d + 1);
        dout_d <= #1 fin_out[doutix_d+1];
      end
   
      // At last output make sure that any pending transactions
      // are sent without any delay. If there is a new output
      // to be sent, no point in removing pushout.
      if(doutix_d == 7) begin
        doutix_d <= #1 0;
        if(rnd!=7 || !pend_dat) begin
          pushout_d <= #1 0;
        end
      end
    
    end 
  end

  always @(*) begin
    if(theta_st) begin
    for(r=0;r<3;r++) begin
      // Use input from Seq block for first round
      // Use output from Tou stage otherwise
      if(r==0) begin
        t_in=dat;
      end else begin
        t_in=rnd_nxt;
      end
      // Begin Theta stage
      for(i=0;i<5;i++) begin
        for(j=0;j<64;j++) begin
          c[i][j]=t_in[0][i][j] ^ t_in[1][i][j] ^ t_in[2][i][j] ^ t_in[3][i][j] ^ t_in[4][i][j]; 
        end
      end
      for(i=0;i<5;i++) begin
        for(j=0;j<64;j++) begin
          d[i][j]=c[modulus((i-1),5)][j] ^ c[modulus((i+1),5)][modulus((j-1),64)]; 
        end
      end
      for(i=0;i<5;i++) begin
        for(j=0;j<5;j++) begin
          for(k=0;k<64;k++) begin
            t_out[j][i][k]=t_in[j][i][k] ^ d[i][k]; 
          end
        end
      end
      // End Theta stage

      // Begin Ro stage
      ro_in=t_out;
      for(k=0;k<64;k++) begin
        ro_out[0][0][k]=t_out[0][0][k]; 
      end
      i=1;
      j=0;
      for(t=0;t<24;t++) begin
        for(k=0;k<64;k++) begin
          ro_out[j][i][k]=ro_in[j][i][modulus((k-((t+1)*(t+2)/2)),64)];
        end
        t_1=i;
        i=j;
        j=modulus((2*t_1+3*j),5);    
      end 
      // End Ro stage

      // Begin Pi stage
      pi_in=ro_out;
      for(i=0;i<5;i++) begin
        for(j=0;j<5;j++) begin
          for(k=0;k<64;k++) begin
            pi_out[i][j][k]=pi_in[j][modulus(j+3*i,5)][k]; 
          end
        end
      end
      // End Pi stage

      // Begin Chi stage
      si_in=pi_out;
      for(i=0;i<5;i++) begin
        for(j=0;j<5;j++) begin
          for(k=0;k<64;k++) begin
            si_out[i][j][k]=si_in[i][j][k] ^ (((si_in[i][modulus((j+1),5)][k])^1)*(si_in[i][modulus((j+2),5)][k])); 
          end
        end
      end
      // End Chi stage

      // Begin Tou stage
      tou_in=si_out;
      tou_out=tou_in;
      R=RC[(rnd*3)+r];
      for(k=0;k<64;k++) begin
        tou_out[0][0][k]=tou_out[0][0][k]^R[k]; 
      end
      // End Tou stage

      // Pass output data to next round
      rnd_nxt=tou_out;
    end
    // Pass the output of 3 rounds to Seq block
    rnd_out=rnd_nxt;
    end else begin
      rnd_out=0;
    end
  end
endmodule
