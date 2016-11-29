-------------------------------------------------------------------------
-- Datapath structural description
-------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_signed.all; -- needed for comparison instructions SLTxx
use work.p_MR2.all;

entity datapath is
	port
	(
		ck, rst: in std_logic;
		i_address: out reg32;
		instruction: in reg32;
		d_address: out reg32;
		data: inout reg32;
		uins: in microinstruction;
		IR_OUT:	out reg32
	);
end datapath;

architecture datapath of datapath is

	signal incpc, pc, npc, IR,  result, R1, R2, RS, RT, RIN,
			ext16, cte_im, IMED, op1, op2, outalu, RALU, MDR,
			mdr_int, dtpc: reg32 := (others =>  '0');
	signal adD, adS: std_logic_vector(4 downto 0):= (others => '0');
	signal inst_branch, inst_grupo1, inst_grupoI: std_logic;
	signal salta: std_logic:= '0';
	alias ixRS: std_logic_vector(4 downto 0) is IR(25 downto 21);	--  index to Rs --
	alias ixRT: std_logic_vector(4 downto 0) is IR(20 downto 16);	--  index to Rt --

begin

	-- auxiliary signals
	inst_branch <= '1' when uins.i=BEQ or uins.i=BGEZ or uins.i=BLEZ or uins.i=BNE else
				'0';
	inst_grupo1 <= '1' when uins.i=ADDU or uins.i=SUBU or uins.i=AAND or uins.i=OOR or uins.i=XXOR else
				'0';
	inst_grupoI <= '1' when uins.i=ADDIU or uins.i=ANDI or uins.i=ORI or uins.i=XORI else
				'0';

	--==============================================================================
	-- first_stage
	--==============================================================================

	incpc <= pc + 4;
	RNPC: entity work.regnbit generic map(N=>32) port map(ck=>ck, rst=>rst, ce=>uins.CY1, D=>incpc, Q=>npc);
	RIR:  entity work.regnbit generic map(N=>32) port map(ck=>ck, rst=>rst, ce=>uins.CY1, D=>instruction, Q=>IR);
	IR_OUT <= ir;	-- IR is the datapath output signal to carry the instruction
	i_address <= pc;  -- connects PC output to the instruction memory address bus

	--==============================================================================
	-- second stage
	--==============================================================================

	-- signal to be written into the register bank
	RIN <= npc when uins.i=JALR else result;

	-- register bank write address selection
	adD <= 	IR(15 downto 11) when inst_grupo1='1' or uins.i=SLTU or uins.i=SLT or uins.i=JALR else
			IR(20 downto 16); -- inst_grupoI='1' or uins.i=SLTIU or uins.i=SLTI or uins.i=LW or  uins.i=LBU  or uins.i=LUI, or default

	adS <= IR(20 downto 16) when uins.i=SSLL or uins.i=SSRL else -- only for shifts
	       IR(25 downto 21); -- this is the default

	REGS: entity work.reg_bank port map (ck => ck, rst => rst, wreg => uins.wreg, AdRs => adS, AdRt => ir(20 downto 16), adRD => adD, RD => RIN, R1 => R1, R2 => R2);
	-- sign extension
	ext16 <= x"FFFF" & IR(15 downto 0) when IR(15)='1' else x"0000" & IR(15 downto 0);
	-- Immediate constant
	cte_im <= ext16(29 downto 0)  & "00"	when inst_branch='1'	else
			-- branch address adjustment for word frontier
			"0000" & IR(25 downto 0) & "00" when uins.i=J  else
				-- J is word addressed. MSB four bits are defined at the ALU, not here!
			x"0000" & IR(15 downto 0) when uins.i=ANDI or uins.i=ORI or uins.i=XORI else
				-- logic instructions with immediate operand are zero extended
			ext16;
				-- The default case is used by addiu, lbu, lw, sbu and sw instructions
	-- second stage registers
	REG_A:  entity work.regnbit generic map(N=>32) port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R1,	    Q=>RS);
	REG_B:  entity work.regnbit generic map(N=>32) port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R2,	    Q=>RT);
	REG_IM: entity work.regnbit generic map(N=>32) port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>cte_im, Q=>IMED);

  --==============================================================================
	-- third stage
	--==============================================================================

	-- select the first ALU operand
	op1 <= pc when inst_branch='1' else	RS;
	-- select the second ALU operand
	op2 <= RT when inst_grupo1='1' or uins.i=SLTU or uins.i=SLT or uins.i=JR else IMED;
	-- ALU instantiation
	inst_alu: entity work.alu port map(op1 => op1, op2 => op2, outalu => outalu, op_alu => uins.i);
	-- ALU registes
	REG_alu: entity work.regnbit generic map(N=>32) port map(ck=>ck, rst=>rst, ce=>uins.wula, D=>outalu, Q=>RALU);
	-- contition to take the branch instructions
	salta <= '1' when ( (RS=RT  and uins.i=BEQ)  or (RS>=0  and uins.i=BGEZ) or
					    (RS <= 0  and uins.i=BLEZ) or (RS/=RT and uins.i=BNE) )  else
			'0';
	result <= MDR when uins.i=LW or uins.i=LBU else RALU;

	--==============================================================================
	-- fourth stage
	--==============================================================================

	d_address <= RALU;
	-- tristate to control memory write
	data <= RT when (uins.ceRW='1' and uins.rw='0') else (others => 'Z');

	-- single byte reading from memory
	mdr_int <= data when uins.i=LW  else x"000000" & data(7 downto 0);
	RMDR: entity work.regnbit generic map(N=>32) port map(ck=>ck, rst=>rst, ce=>uins.wmdr, D=>mdr_int, Q=>MDR);

	--==============================================================================
	-- fifth stage
	--==============================================================================

	dtpc <= result when (inst_branch='1' and salta='1') or uins.i=J or uins.i=JALR or uins.i=JR  else npc;

	--  Data memory starting address: beware of the OFFSET!
	rpc: entity work.regnbit generic map(N=>32, INIT_VALUE=>x"00400000") port map(ck=>ck, rst=>rst, ce=>uins.wpc, D=>dtpc, Q=>pc);

end datapath;