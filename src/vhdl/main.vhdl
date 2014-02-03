library ieee;
use ieee.std_logic_1164.all;
use work.all;

entity main is
  port(
    CLK_32MHZ: in std_logic;
    BTN_UP : in std_logic;
    BTN_DOWN : in std_logic;
    BTN_LEFT : in std_logic;
    BTN_RIGHT : in std_logic;
    BTN_RESET : in std_logic;
    LED : out std_logic_vector(3 downto 0);
    VGA_VSYNC : out std_logic;
    VGA_HSYNC : out std_logic;
    VGA_R : out std_logic_vector(3 downto 0);
    VGA_G : out std_logic_vector(3 downto 0);
    VGA_B : out std_logic_vector(3 downto 0)
    );
end main;

architecture arch of main is
  signal CLK_50MHZ : std_logic;
  
  component dcm
    port(
      CLK_32MHZ : in std_logic;          
      CLK_50MHZ : out std_logic
      );
  end component;

  component Video
    port(CLK_50MHZ : in std_logic;
         BTN_UP : in std_logic;
         BTN_DOWN : in std_logic;
         BTN_LEFT : in std_logic;
         BTN_RIGHT : in std_logic;
         BTN_RESET : in std_logic;
         LED : out std_logic_vector(3 downto 0);
         VGA_VSYNC : out std_logic;
         VGA_HSYNC : out std_logic;
         VGA_R : out std_logic_vector(3 downto 0);
         VGA_G : out std_logic_vector(3 downto 0);
         VGA_B : out std_logic_vector(3 downto 0)
         );
    end component;
  
begin
  inst_dcm: dcm
    port map(
      CLK_32MHZ => CLK_32MHZ,
      CLK_50MHZ => CLK_50MHZ
      );

  inst_Video: Video
    port map(
      CLK_50MHZ => CLK_50MHZ,
      BTN_UP => BTN_UP,
      BTN_DOWN => BTN_DOWN,
      BTN_LEFT => BTN_LEFT,
      BTN_RIGHT => BTN_RIGHT,
      BTN_RESET => BTN_RESET,
      LED => LED,
      VGA_VSYNC => VGA_VSYNC,
      VGA_HSYNC => VGA_HSYNC,
      VGA_R => VGA_R,
      VGA_G => VGA_G,
      VGA_B => VGA_B
      );
  
end arch;
