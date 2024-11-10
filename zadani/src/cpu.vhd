-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): xhricma00 <login AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru

   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0);-- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti

   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data

   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;               -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is


  -- CNT signals
  signal CNT      : std_logic_vector(7 downto 0);
  signal CNT_INC  : std_logic;
  signal CNT_DEC  : std_logic;
  signal CNT_ZERO : std_logic;

  -- TMP signals
  signal TMP      : std_logic_vector(7 downto 0);
  signal TMP_ID   : std_logic;

  -- PC signals
  signal PC       : std_logic_vector(12 downto 0);
  signal PC_INC   : std_logic;
  signal PC_DEC   : std_logic;

  -- PTR signals
  signal PTR      : std_logic_vector(12 downto 0);
  signal PTR_INC  : std_logic;
  signal PTR_DEC  : std_logic;

  -- multiplexer signals
  signal ADDR_MUX_SEL : std_logic;
  signal WSRC_MUX_SEL : std_logic_vector(1 downto 0);

  type state_type is (s_reset,
   nr_idle, nr_fetch, nr_decode,
   idle, fetch, decode,
   inc,
   dec,
   add_load, add_store,
   sub_load, sub_store,
   ls, ls_val_load, ls_val_check, ls_skip, ls_skip_check, ls_skip_ins_load, ls_skip_ins_check, ls_skip_ins_check_inc, ls_skip_ins_check_dec,
   le_val_load, le_val_check, le_val_check_end, le_go_back, le_go_back_check, le_go_back_ins_load, le_go_back_ins_check, le_go_back_ins_check_inc, le_go_back_ins_check_dec,
   load_tmp_load, load_tmp,
   store_tmp_load, store_tmp,
   print_wait, print_load, print,
   read_wait, read_in,
   noop,
   halt);

  signal STATE                      : state_type := s_reset;
  signal N_STATE                    : state_type;
  attribute fsm_encoding            : string;
  attribute fsm_encoding of STATE   : signal is "sequential";
  attribute fsm_encoding of N_STATE : signal is "sequential";

  begin

  PROCESS_PC : process (CLK, RESET)
  begin

    if (RESET = '1') then
        PC <= (others => '0');
    elsif rising_edge(CLK) then
      if (PC_INC = '1') then
        PC <= PC + 1;
      elsif (PC_DEC = '1') then
        PC <= PC - 1;
      end if;
    end if;

    end process;

  PROCESS_PTR : process (CLK, RESET)
  begin

    if (RESET = '1') then
      PTR <= (others => '0');
    elsif rising_edge(CLK) then
      if (PTR_INC = '1') then
        PTR <= PTR + 1;
      elsif (PTR_DEC = '1') then
        PTR <= PTR - 1;
      end if;
    end if;

  end process;

  PROCESS_CNT : process (CLK, RESET)
  begin

    if (RESET = '1') then
      CNT <= (others => '0');
    elsif rising_edge(CLK) then
      if (CNT_INC = '1') then
        CNT <= CNT + 1;
      elsif (CNT_DEC = '1') then
        CNT <= CNT - 1;
      end if;
    end if;

  end process;

  PROCESS_CNT_ZERO : process (CNT, RESET)
  begin

    if (RESET = '1') then
      CNT_ZERO <= '1';
    else
      if (CNT = X"00") then
        CNT_ZERO <= '1';
      else
        CNT_ZERO <= '0';
      end if;
    end if;

  end process;

  PROCESS_TMP : process (CLK, RESET)
  begin

  if (RESET = '1') then
    TMP <= (others => '0');
  elsif rising_edge(CLK) then
    if (TMP_ID = '1') then
      TMP <= DATA_RDATA;
    end if;
  end if;

  end process;

  PROCESS_ADDR_MUX : process (ADDR_MUX_SEL, PTR, PC)
  begin

  case ADDR_MUX_SEL is
    when '0' =>
      DATA_ADDR <= PTR;
    when '1' =>
      DATA_ADDR <= PC;
    when others => null;
  end case;

  end process;

  PROCESS_WSRC_MUX : process (WSRC_MUX_SEL, DATA_RDATA, IN_DATA, TMP)
  begin

  case WSRC_MUX_SEL is
    when "00" =>
      DATA_WDATA <= IN_DATA;
    when "01" =>
      DATA_WDATA <= TMP;
    when "10" =>
      DATA_WDATA <= DATA_RDATA - 1;
    when "11" =>
      DATA_WDATA <= DATA_RDATA + 1;
    when others => null;
  end case;

  end process;

  FSM_STATE_CHANGE : process (CLK, RESET)
  begin

    if (RESET = '1') then
      STATE <= s_reset;
    elsif rising_edge(CLK) then
      STATE <= N_STATE;
    end if;

  end process;

  FSM : process (STATE, EN, DATA_RDATA, IN_VLD, OUT_BUSY)
  begin
    case STATE is
      when s_reset =>
        N_STATE <= nr_idle;

      when nr_idle =>
        if (EN = '1') then
          N_STATE <= nr_fetch;
        else
          N_STATE <= nr_idle;
        end if;

      when nr_fetch =>
        N_STATE <= nr_decode;

      when nr_decode =>
        if (DATA_RDATA = X"40") then
          N_STATE <= idle;
        else
          N_STATE <= nr_fetch;
        end if;

      when idle =>
        if (EN = '1') then
          N_STATE <= fetch;
        else
          N_STATE <= idle;
        end if;

      when fetch =>
        N_STATE <= decode;

      when decode =>
        case DATA_RDATA is
          when X"3E" => -- >
            N_STATE <= inc;
          when X"3C" => -- <
            N_STATE <= dec;
          when X"2B" => -- +
            N_STATE <= add_load;
          when X"2D" => -- -
            N_STATE <= sub_load;
          when X"5B" => -- [
            N_STATE <= ls;
          when X"5D" => -- ]
            N_STATE <= le_val_load;
          when X"24" => -- $
            N_STATE <= load_tmp_load;
          when X"21" => -- !
            N_STATE <= store_tmp_load;
          when X"2E" => -- .
            N_STATE <= print_wait;
          when X"2C" => -- ,
            N_STATE <= read_wait;
          when X"40" => -- @
            N_STATE <= halt;
          when others => -- comments
            N_STATE <= noop;
        end case;

      when inc =>
        N_STATE <= fetch;

      when dec =>
        N_STATE <= fetch;

      when add_load =>
        N_STATE <= add_store;

      when add_store =>
        N_STATE <= fetch;

      when sub_load =>
        N_STATE <= sub_store;

      when sub_store =>
        N_STATE <= fetch;




      when ls => -- [ ; pc++
        N_STATE <= ls_val_load;

      when ls_val_load => -- load value from memory
        N_STATE <= ls_val_check;

      when ls_val_check => -- check if value == 0
        if (DATA_RDATA = X"00") then
          N_STATE <= ls_skip;
        else
          N_STATE <= fetch; -- continue execution
        end if;

      when ls_skip => -- start skip process ; cnt = 1
        N_STATE <= ls_skip_check;

      when ls_skip_check => -- check if cnt == 0
        if (CNT_ZERO = '1') then
          N_STATE <= fetch; -- continue execution
        else
          N_STATE <= ls_skip_ins_load;
        end if;

      when ls_skip_ins_load => -- load instruction, should also increment pc
        N_STATE <= ls_skip_ins_check;

      when ls_skip_ins_check => -- check if instruction is [ or ]
        if (DATA_RDATA = X"5B") then -- [
          N_STATE <= ls_skip_ins_check_inc;
        elsif (DATA_RDATA = X"5D") then -- ]
          N_STATE <= ls_skip_ins_check_dec;
        else
          N_STATE <= ls_skip_check;
        end if;

      when ls_skip_ins_check_inc => -- [ ; cnt++
        N_STATE <= ls_skip_check;

      when ls_skip_ins_check_dec => -- ] ; cnt--
        N_STATE <= ls_skip_check;


      when le_val_load => -- ] ;
          N_STATE <= le_val_check;

      when le_val_check => -- check if value == 0
        if (DATA_RDATA = X"00") then
          N_STATE <= le_val_check_end; -- continue execution
        else
          N_STATE <= le_go_back;
        end if;

      when le_val_check_end => -- pc++
        N_STATE <= fetch;

      when le_go_back => -- cnt = 1, pc--
        N_STATE <= le_go_back_check;

      when le_go_back_check => -- check if cnt == 0
        if (CNT_ZERO = '1') then
          N_STATE <= fetch; -- continue execution
        else
          N_STATE <= le_go_back_ins_load;
        end if;

      when le_go_back_ins_load => -- load instruction, should also decrement pc
        N_STATE <= le_go_back_ins_check;

      when le_go_back_ins_check => -- check if instruction is [ or ]
        if (DATA_RDATA = X"5B") then -- [
          N_STATE <= le_go_back_ins_check_inc;
        elsif (DATA_RDATA = X"5D") then -- ]
          N_STATE <= le_go_back_ins_check_dec;
        else
          N_STATE <= le_go_back_check;
        end if;

      when le_go_back_ins_check_inc => -- [ ; cnt++
        N_STATE <= le_go_back_check;

      when le_go_back_ins_check_dec => -- ] ; cnt--
        N_STATE <= le_go_back_ins_check;



















      when load_tmp_load =>
        N_STATE <= load_tmp;

      when load_tmp =>
        N_STATE <= fetch;

      when store_tmp_load =>
        N_STATE <= store_tmp;

      when store_tmp =>
        N_STATE <= fetch;

      when print_wait =>
        if (OUT_BUSY = '0') then
          N_STATE <= print_load;
        else
          N_STATE <= print_wait;
        end if;

      when print_load =>
        N_STATE <= print;

      when print =>
        N_STATE <= fetch;

      when read_wait =>
        if (IN_VLD = '1') then
          N_STATE <= read_in;
        else
          N_STATE <= read_wait;
        end if;

      when read_in =>
        N_STATE <= fetch;

      when noop =>
        N_STATE <= fetch;

      when halt =>
        N_STATE <= idle;
    end case;

  end process;

  output : process (STATE)
  begin
    DATA_EN <= '0';

    IN_REQ <= '0';
    OUT_INV <= '0';
    OUT_WE <= '0';
    OUT_DATA <= (others => '0');
    DATA_RDWR <= '1';

    ADDR_MUX_SEL <= '0';
    WSRC_MUX_SEL <= "00";

    CNT_INC <= '0';
    CNT_DEC <= '0';
    PC_INC <= '0';
    PC_DEC <= '0';
    PTR_INC <= '0';
    PTR_DEC <= '0';
    TMP_ID <= '0';

    case STATE is
      when s_reset =>
        READY <= '0';
        DONE <= '0';

      when nr_idle =>
        null;

      when nr_fetch =>
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        PTR_INC <= '1';

      when nr_decode =>
        null;

      when idle =>
        READY <= '1';

      when fetch =>
        ADDR_MUX_SEL <= '1';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when decode =>
        null;

      when inc =>
        PTR_INC <= '1';
        PC_INC <= '1';

      when dec =>
        PTR_DEC <= '1';
        PC_INC <= '1';

      when add_load => null;
        PC_INC <= '1';
        ADDR_MUX_SEL <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';

      when add_store => null;
        ADDR_MUX_SEL <= '0';
        WSRC_MUX_SEL <= "11";
        DATA_RDWR <= '0';
        DATA_EN <= '1';

      when sub_load => null;
        PC_INC <= '1';
        ADDR_MUX_SEL <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';

      when sub_store => null;
        ADDR_MUX_SEL <= '0';
        WSRC_MUX_SEL <= "10";
        DATA_RDWR <= '0';
        DATA_EN <= '1';




      when ls =>
        PC_INC <= '1';

      when ls_val_load =>
        ADDR_MUX_SEL <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';

      when ls_val_check =>
        null;

      when ls_skip =>
        CNT_INC <= '1';

      when ls_skip_check =>
        null;

      when ls_skip_ins_load =>
        PC_INC <= '1';
        ADDR_MUX_SEL <= '1';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when ls_skip_ins_check =>
        null;

      when ls_skip_ins_check_inc =>
        CNT_INC <= '1';

      when ls_skip_ins_check_dec =>
        CNT_DEC <= '1';













      when le_val_load =>
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when le_val_check =>
        null;

      when le_val_check_end =>
        PC_INC <= '1';

      when le_go_back =>
        CNT_INC <= '1';
        PC_DEC <= '1';

      when le_go_back_check =>
        null;

      when le_go_back_ins_load =>
        PC_DEC <= '1';
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when le_go_back_ins_check =>
        null;

      when le_go_back_ins_check_inc =>
        CNT_INC <= '1';

      when le_go_back_ins_check_dec =>
        CNT_DEC <= '1';






















      when load_tmp_load =>
        PC_INC <= '1';
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when load_tmp =>
        TMP_ID <= '1';

      when store_tmp_load =>
        PC_INC <= '1';
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when store_tmp =>
        ADDR_MUX_SEL <= '0';
        WSRC_MUX_SEL <= "01";
        DATA_EN <= '1';
        DATA_RDWR <= '0';


      when print_wait =>
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when print_load =>
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';

      when print =>
        PC_INC <= '1';
        OUT_WE <= '1';
        OUT_DATA <= DATA_RDATA;

      when read_wait =>
        IN_REQ <= '1';

      when read_in =>
        PC_INC <= '1';
        ADDR_MUX_SEL <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        WSRC_MUX_SEL <= "00";

      when noop =>
        PC_INC <= '1';

      when halt =>
        DONE <= '1';
  end case;

  end process;

end behavioral;
