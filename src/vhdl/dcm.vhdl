library ieee;
use ieee.std_logic_1164.all;
use work.all;

entity dcm is
  port(
    CLK_32MHZ: in std_logic;
    CLK_50MHZ: out std_logic
    );
end dcm;

architecture arch of dcm is
begin
  inst_dcm_32_to_50p35: entity work.dcm_32_to_50p35
    port map(
      clkin_in => CLK_32MHZ,
      clkfx_out => CLK_50MHZ,
      clkin_ibufg_out => open,
      clk0_out => open
      );
end arch;
