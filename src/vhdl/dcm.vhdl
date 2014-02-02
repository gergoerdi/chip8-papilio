library ieee;
use ieee.std_logic_1164.all;

entity dcm is
  port(
    CLK_32MHZ: in std_logic;
    CLK_50MHZ: out std_logic
    );
end dcm;

architecture arch of dcm is
  component dcm_32_to_50p35
    port(
      clkin_in : in std_logic;          
      clkfx_out : out std_logic;
      clkin_ibufg_out : out std_logic;
      clk0_out : out std_logic
      );
  end component;
begin
  inst_dcm_32_to_50p35: dcm_32_to_50p35
    port map(
      clkin_in => CLK_32MHZ,
      clkfx_out => CLK_50MHZ,
      clkin_ibufg_out => open,
      clk0_out => open
      );
end arch;
