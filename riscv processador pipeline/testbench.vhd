library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library std;
use std.textio.all;

entity testbench is
end testbench;

architecture Behavioral of testbench is
    signal clock_s: std_logic;
    
	signal next_address_s: std_logic_vector(31 downto 0);
    
	signal address_0_s: std_logic_vector(31 downto 0);
	signal address_1_s: std_logic_vector(31 downto 0);
	signal address_2_s: std_logic_vector(31 downto 0);
	signal address_3_s: std_logic_vector(31 downto 0);
	signal address_4_s: std_logic_vector(31 downto 0);
	signal address_5_s: std_logic_vector(31 downto 0);
    
    signal address_plus4_s: std_logic_vector(31 downto 0);
    signal address_j_s: std_logic_vector(31 downto 0);
    signal address_j_1_s: std_logic_vector(31 downto 0);
    
    signal we_s: std_logic;
    signal dataout_s: std_logic_vector(31 downto 0);
    
    signal rs1_s: std_logic_vector(4 downto 0);
    signal instruction_s: std_logic_vector(31 downto 0);
    
    signal ALUSrc_0_s: std_logic;
    signal ALUOp_0_s: std_logic_vector(1 downto 0);
    signal Branch_0_s: std_logic;
    signal MemRead_0_s: std_logic;
    signal MemWrite_0_s: std_logic;
    signal RegWrite_0_s: std_logic;
    signal Mem2Reg_0_s: std_logic;
    
    signal ALUSrc_1_s: std_logic;
    signal ALUOp_1_s: std_logic_vector(1 downto 0);
    signal Branch_1_s: std_logic;
    signal MemRead_1_s: std_logic;
    signal MemWrite_1_s: std_logic;
    signal RegWrite_1_s: std_logic;
    signal Mem2Reg_1_s: std_logic;
    
    signal Branch_2_s: std_logic;
    signal MemRead_2_s: std_logic;
    signal MemWrite_2_s: std_logic;
    signal RegWrite_2_s: std_logic;
    signal Mem2Reg_2_s: std_logic;
    
    signal RegWrite_3_s: std_logic;
    signal Mem2Reg_3_s: std_logic;
    
    signal rst_regs_s: std_logic;
    signal data_s: std_logic_vector(31 downto 0);
    signal ro1_0_s: std_logic_vector(31 downto 0);
    signal ro2_0_s: std_logic_vector(31 downto 0);
    
    signal ro1_1_s: std_logic_vector(31 downto 0);
    signal ro2_1_s: std_logic_vector(31 downto 0);
    signal ro2_2_s: std_logic_vector(31 downto 0);
    
    signal imm32_0_s: signed(31 downto 0);
    signal imm32_1_s: std_logic_vector(31 downto 0);
    
    
    signal funct3_s: std_logic_vector(2 downto 0);
    signal funct7_s: std_logic_vector(6 downto 0);
    
    signal destReg_0_s: std_logic_vector(4 downto 0);
    signal destReg_1_s: std_logic_vector(4 downto 0);
    signal destReg_2_s: std_logic_vector(4 downto 0);
    
    signal ro2_ALU_s: std_logic_vector(31 downto 0);
    signal instr_ALU_s: std_logic_vector(3 downto 0);
    
    signal ALU_res_0_s: std_logic_vector(31 downto 0);
    
    signal ALU_res_1_s: std_logic_vector(31 downto 0);
    signal zero_1_s: std_logic;
    
    signal ALU_res_2_s: std_logic_vector(31 downto 0);
    
    signal ram_data_0_s: std_logic_vector(31 downto 0);
    signal ram_data_1_s: std_logic_vector(31 downto 0);

    signal incondicional_0_s: std_logic;
    signal incondicional_1_s: std_logic;
    signal incondicional_2_s: std_logic; 
    
	component Add is
		port(
        	val1	: in std_logic_vector(31 downto 0);
        	val2	: in std_logic_vector(31 downto 0);

        	result	: out std_logic_vector(31 downto 0)
    	);
	end component;
  
	component MUX is
    	port(
        	caso1	: in std_logic_vector(31 downto 0);
        	caso2	: in std_logic_vector(31 downto 0);
        	decisor	: in std_logic;

        	saida: out std_logic_vector(31 downto 0)
        );
	end component;
  
	component PC is
		port(
		clk: in std_logic;

        	recieved: in std_logic_vector(31 downto 0);
        
        	address	: out std_logic_vector(31 downto 0)
    	);
	end component;
    
    component mem_rom is
    	port (
        	clock: in std_logic;
        	we: in std_logic;
        	address: in std_logic_vector(7 downto 0);

        	dataout: out std_logic_vector(31 downto 0)
        );
    end component;
    
    component regIFID is
    	port(
            clk	:	 in std_logic;
            PC		: in std_logic_vector(31 downto 0);
            ins		: in std_logic_vector(31 downto 0);

            rs1Prev	: out std_logic_vector(4 downto 0);
            PCPrev	: out std_logic_vector(31 downto 0);
            insPrev	: out std_logic_vector(31 downto 0)
    	);
	end component;
  
	component control is
		port(
        	instruction					: in std_logic_vector(31 downto 0);
        	
        	ALUSrc						: out std_logic;
        	ALUOp						: out std_logic_vector(1 downto 0);
        	Branch, MemRead, MemWrite	: out std_logic;
        	RegWrite, Mem2Reg			: out std_logic;
	        incondicional				: out std_logic
		);
	end component;
    
    component XREGS is
      	generic (WSIZE : natural := 32);
      	port (
        	clk, wren, rst	: in std_logic;
            rs1, rs2, rd	: in std_logic_vector(4 downto 0);
            data			: in std_logic_vector(WSIZE-1 downto 0);

            ro1, ro2		: out std_logic_vector(WSIZE-1 downto 0)
        );
  	end component;
    
    component genImm32 is
		port (
  			instr : in std_logic_vector(31 downto 0);
  			imm32 : out signed(31 downto 0));
	end component;
    
    component regIDEX is
      port(
          clk										: in std_logic;
          PC										: in std_logic_vector(31 downto 0);
          ro1, ro2 								: in std_logic_vector(31 downto 0);
          imm32									: in std_logic_vector(31 downto 0);
          instruction								: in std_logic_vector(31 downto 0);
          ALUSrc									: in std_logic;
          ALUOp									: in std_logic_vector(1 downto 0);
          Branch, MemRead, MemWrite				: in std_logic;
          RegWrite, Mem2Reg						: in std_logic;
	  incondicional								: in std_logic;
	

          PCPrev									: out std_logic_vector(31 downto 0);
          ro1Prev, ro2Prev 						: out std_logic_vector(31 downto 0);
          imm32Prev								: out std_logic_vector(31 downto 0);
          funct3Prev								: out std_logic_vector(2 downto 0);
          funct7Prev								: out std_logic_vector(6 downto 0);
          destRegPrev								: out std_logic_vector(4 downto 0);
          --EX
          ALUSrcPrev								: out std_logic;
          ALUOpPrev								: out std_logic_vector(1 downto 0);
          --M
          BranchPrev, MemReadPrev, MemWritePrev	: out std_logic;
          --WB
          RegWritePrev, Mem2RegPrev				: out std_logic;
	  incondicionalPrev							: out std_logic
      );
  end component;
  
  component ALU_control is
      port(
          ALUOp	: in std_logic_vector(1 downto 0);
          funct7	: in std_logic_vector(6 downto 0);
          funct3	: in std_logic_vector(2 downto 0);
          instr	: out std_logic_vector(3 downto 0)
      );
  end component;
  
  component ula_RV is
      generic(WSIZE: natural := 32);
      port(
          opcode: in std_logic_vector(3 downto 0);
          A,B: in std_logic_vector(WSIZE-1 downto 0);
          Z: out std_logic_vector(WSIZE-1 downto 0)
      );
  end component;
  
  component regEXMEM is
      port(
          clk										: in std_logic;
          PC										: in std_logic_vector(31 downto 0);
          PCAdd									: in std_logic_vector(31 downto 0);
          ALUresult								: in std_logic_vector(31 downto 0);
          destReg									: in std_logic_vector (4 downto 0);
          ro2 									: in std_logic_vector(31 downto 0);
          Zero									: in std_logic;
          Branch, MemRead, MemWrite				: in std_logic;
          RegWrite, Mem2Reg						: in std_logic;
	incondicional							: in std_logic;

          PCPrev									: out std_logic_vector(31 downto 0);
          PCAddPrev								: out std_logic_vector(31 downto 0);
          ALUresultPrev							: out std_logic_vector(31 downto 0);
          destRegPrev								: out std_logic_vector (4 downto 0);
          ro2Prev 								: out std_logic_vector(31 downto 0);
          ZeroPrev								: out std_logic;
          --M
          BranchPrev, MemReadPrev, MemWritePrev	: out std_logic;
          --WB
          RegWritePrev, Mem2RegPrev				: out std_logic;
	  incondicionalPrev							: out std_logic
      );
  end component;
  
	component mem_ram is
    	port (
        	clk: in std_logic;
        	we: in std_logic;
        	address: in std_logic_vector(7 downto 0);
        	datain: in std_logic_vector(31 downto 0);
        	dataout: out std_logic_vector(31 downto 0)
      	);
	end component;
    
    component regMEMWB is
      port(
          clk										: in std_logic;
          PC										: in std_logic_vector(31 downto 0);
          PCAdd									: in std_logic_vector(31 downto 0);
          ALUresult								: in std_logic_vector(31 downto 0);
          Data									: in std_logic_vector(31 downto 0);
          destReg									: in std_logic_vector (4 downto 0);
          --WB
          RegWrite, Mem2Reg						: in std_logic;

          PCPrev									: out std_logic_vector(31 downto 0);
          PCAddPrev								: out std_logic_vector(31 downto 0);
          ALUresultPrev							: out std_logic_vector(31 downto 0);
          DataPrev								: out std_logic_vector(31 downto 0);
          destRegPrev								: out std_logic_vector (4 downto 0);
          --WB
          RegWritePrev, Mem2RegPrev				: out std_logic
      );
  end component;

  	type rom_type is array (0 to 255) of std_logic_vector(31 downto 0);
	impure function read_rom_data return rom_type is
        file rom_file: text open read_mode is "rom_data.txt";
        variable txt_line: line;
    	variable rom_data: rom_type;

    begin
        for i in 0 to 255 loop
        	readline(rom_file, txt_line);
          	hread(txt_line, rom_data(i));
        end loop;
        return rom_data;
    end function;

  	signal rom: rom_type:= read_rom_data;
  --constant rom: rom_type := read_rom_data("rom_data.txt");

begin
    
    mux1: MUX
    port map (
    	caso1 => address_plus4_s,
        caso2 => address_j_s,
        decisor => ((Branch_2_s and zero_1_s) or incondicional_2_s),
        saida => next_address_s
    );
    
    counter: PC
    port map (
	clk => clock_s,
      recieved => next_address_s,
      address => address_0_s
    );
    
    add1: Add
    port map (
    	val1 => address_0_s,
        val2 => X"00000004",
        result => address_plus4_s
    );
    
    
    uut: mem_rom
    port map (
      clock => clock_s,
      we => we_s,
      address => address_0_s(7 downto 0),
      dataout => dataout_s
    );
    
    ifid: regIFID
    port map (
    	clk => clock_s,
        PC => address_0_s,
        ins => dataout_s,
        PCPrev => address_1_s,
        rs1Prev => rs1_s,
        insPrev => instruction_s
    );
    
    ctrl: control
    port map (
    	instruction => instruction_s,
        ALUSrc => ALUSrc_0_s,
        ALUOp => ALUOp_0_s,
        Branch => Branch_0_s,
        MemRead => MemRead_0_s,
        MemWrite => MemWrite_0_s,
        RegWrite => RegWrite_0_s,
        Mem2Reg => Mem2Reg_0_s,
	incondicional => incondicional_0_s
    );
    
    regs: XREGS
    port map (
      clk => clock_s,
      wren => RegWrite_3_s,
      rst => rst_regs_s,
      rs1 => rs1_s,
      rs2 => instruction_s(24 downto 20),
      rd => destReg_2_s,
      data => data_s,
      ro1 => ro1_0_s,
      ro2 => ro2_0_s
    );
    
    geraimm: genImm32
    port map (
    	instr => instruction_s,
        imm32 => imm32_0_s
    );
    
    idex : regIDEX
    port map (
    	clk => clock_s,
        PC => address_1_s,
        ro1 => ro1_0_s,
        ro2 => ro2_0_s,
        imm32 => std_logic_vector(imm32_0_s),
        instruction => instruction_s,
        ALUSrc => ALUSrc_0_s,
        ALUOp => ALUOp_0_s,
        Branch => Branch_0_s,
        MemRead => MemRead_0_s,
        MemWrite => MemWrite_0_s,
        RegWrite => RegWrite_0_s,
        Mem2Reg => Mem2Reg_0_s,
	incondicional => incondicional_0_s,

        PCPrev => address_2_s,
        ro1Prev => ro1_1_s,
        ro2Prev => ro2_1_s,
        imm32Prev => imm32_1_s,
        funct3Prev => funct3_s,
        funct7Prev => funct7_s,
        destRegPrev => destReg_0_s,        
        ALUSrcPrev => ALUSrc_1_s,
        ALUOpPrev => ALUOp_1_s,
        BranchPrev => Branch_1_s,
        MemReadPrev => MemRead_1_s,
        MemWritePrev => MemWrite_1_s,
        RegWritePrev => RegWrite_1_s,
        Mem2RegPrev => Mem2Reg_1_s,
	incondicionalPrev => incondicional_1_s
    );
    
  	add2: Add
    port map (
    	val1 => address_2_s,
        val2 => std_logic_vector(shift_left(unsigned(imm32_1_s), 1)),
        result => address_3_s
    );
    
    mux2: MUX
    port map (
    	caso1 => ro2_1_s,
        caso2 => imm32_1_s,
        decisor => ALUSrc_1_s,
        saida => ro2_ALU_s
    );
    
    aluctrl: ALU_control
    port map (
    	ALUOp => ALUOp_1_s,
        funct7 => funct7_s,
        funct3 => funct3_s,
        instr => instr_ALU_s
    );
    
    ula: ula_RV
    port map (
    	opcode => instr_ALU_s,
        A => ro1_1_s,
        B => ro2_ALU_s,
        Z => ALU_res_0_s
    );
    
    exmem : regEXMEM
    port map (
    	clk => clock_s,
        PC => address_2_s,
        PCAdd => address_3_s,
        ALUresult => ALU_res_0_s,
        destReg => destReg_0_s,
        ro2 => ro2_1_s,
        Zero => ALU_res_0_s(0),
	incondicional => incondicional_1_s,
        
        Branch => Branch_1_s,
        MemRead => MemRead_1_s,
        MemWrite => MemWrite_1_s,
        RegWrite => RegWrite_1_s,
        Mem2Reg => Mem2Reg_1_s,
        
        PCPrev => address_4_s,
        PCAddPrev => address_j_s,
        ALUresultPrev => ALU_res_1_s,
        destRegPrev => destReg_1_s,
        ro2Prev => ro2_2_s,
        ZeroPrev => zero_1_s,
        
        BranchPrev => Branch_2_s,
        MemReadPrev => MemRead_2_s,
        MemWritePrev => MemWrite_2_s,
        RegWritePrev => RegWrite_2_s,
        Mem2RegPrev => Mem2Reg_2_s,
	
	incondicionalPrev => incondicional_2_s
    );
    
    ram: mem_ram
    port map (
    	clk => clock_s,
        we => MemWrite_2_s,
        address => ALU_res_1_s(7 downto 0),
        datain => ro2_2_s,
        dataout=> ram_data_0_s
    );
    
    memwb: regMEMWB
    port map (
    	clk => clock_s,
        PC => address_4_s,
        PCAdd => address_j_s,
        ALUresult => ALU_res_1_s,
        Data => ram_data_0_s,
        destReg => destReg_1_s,
        RegWrite => RegWrite_2_s,
        Mem2Reg => Mem2Reg_2_s,
        
        PCPrev => address_5_s,
        PCAddPrev => address_j_1_s,
        ALUresultPrev => ALU_res_2_s,
        DataPrev => ram_data_1_s,
        destRegPrev => destReg_2_s,
        RegWritePrev => RegWrite_3_s,
        Mem2RegPrev => Mem2Reg_3_s
    );
    
    mux3: MUX
    port map (
        caso1 => ALU_res_2_s,
    	caso2 => ram_data_1_s,
        decisor => Mem2Reg_3_s,
        saida => data_s
    );

  clk: process
  begin
    clock_s <= '0';
    wait for 5 ns;
    clock_s <= '1';
    wait for 5 ns;
  end process;

  estimulos: process
  begin
    we_s <= '0';
    
    wait;
  end process;

end Behavioral;