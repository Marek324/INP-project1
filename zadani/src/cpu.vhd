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
  signal CNT_SET  : std_logic;

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
   nr_idle, nr_fetch, nr_decode, -- not ready states
   idle, fetch, decode,
   ex_add,
   ex_sub,
   ex_ls, ex_ls_cnt_check, ex_ls_skip, -- loop start
   ex_le, ex_le_back_l, ex_le_back_check, ex_le_back_cnt_check,  -- loop end
   ex_tmp_load,
   ex_tmp_store,
   ex_print_w, ex_print,
   ex_read_w, ex_read,
   halt);

  signal STATE                      : state_type := nr_fetch;
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
      elsif (CNT_SET = '1') then
        CNT <= X"01";
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

    DATA_EN <= '0';

    IN_REQ <= '0';
    OUT_INV <= '0';
    OUT_WE <= '0';
    OUT_DATA <= (others => '0');
    DATA_RDWR <= '1';

    ADDR_MUX_SEL <= '0';
    WSRC_MUX_SEL <= "00";

    CNT_INC <= '0';
    CNT_SET <= '0';
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
        if(EN = '1') then
          N_STATE <= nr_fetch;
        else
          N_STATE <= nr_idle;
        end if;

      when nr_idle =>
        if (EN = '1') then
          N_STATE <= nr_fetch;
        else
          N_STATE <= nr_idle;
        end if;

      when nr_fetch =>
        DATA_EN <= '1';
        PTR_INC <= '1';
        N_STATE <= nr_decode;

      when nr_decode =>
        if (DATA_RDATA = X"40") then
          READY <= '1';
          if (EN = '1') then -- fetch
            ADDR_MUX_SEL <= '1';
            DATA_EN <= '1';
            N_STATE <= decode;
          else
            N_STATE <= idle;
          end if;
        else
          DATA_EN <= '1';
          PTR_INC <= '1';
          N_STATE <= nr_decode;
        end if;

      when idle =>
        if (EN = '1') then -- fetch
          ADDR_MUX_SEL <= '1';
          DATA_EN <= '1';
          N_STATE <= decode;
        else
          N_STATE <= idle;
        end if;

      when fetch =>
        ADDR_MUX_SEL <= '1';
        DATA_EN <= '1';
        N_STATE <= decode;

      when decode =>
        PC_INC <= '1';
        case DATA_RDATA is
          when X"3E" => -- >
            PTR_INC <= '1';
            N_STATE <= fetch;
          when X"3C" => -- <
            PTR_DEC <= '1';
            N_STATE <= fetch;
          when X"2B" => -- +
            DATA_EN <= '1';
            N_STATE <= ex_add;
          when X"2D" => -- -
            DATA_EN <= '1';
            N_STATE <= ex_sub;
          when X"5B" => -- [
            DATA_EN <= '1';
            N_STATE <= ex_ls;
          when X"5D" => -- ]
            DATA_EN <= '1';
            N_STATE <= ex_le;
            PC_INC <= '0';
          when X"24" => -- $
            DATA_EN <= '1';
            N_STATE <= ex_tmp_load;
          when X"21" => -- !
            DATA_EN <= '1';
            N_STATE <= ex_tmp_store;
          when X"2E" => -- .
            DATA_EN <= '1';
            if (OUT_BUSY = '0') then
              N_STATE <= ex_print;
            else
              N_STATE <= ex_print_w;
            end if;
          when X"2C" => -- ,
            IN_REQ <= '1';
            if (IN_VLD = '1') then
              N_STATE <= ex_read;
            else
              N_STATE <= ex_read_w;
            end if;
          when X"40" => -- @
            N_STATE <= halt;
            PC_INC <= '0';
          when others => -- comments
            N_STATE <= fetch;
        end case;

      -- ARITHMETIC
      when ex_add =>
        WSRC_MUX_SEL <= "11";
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        N_STATE <= fetch;

      when ex_sub =>
        WSRC_MUX_SEL <= "10";
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        N_STATE <= fetch;

      -- LOOP
      when ex_ls =>
        if (DATA_RDATA = X"00") then
          CNT_SET <= '1';
          DATA_EN <= '1';
          ADDR_MUX_SEL <= '1';
          N_STATE <= ex_ls_skip;
        else
          ADDR_MUX_SEL <= '1';
          DATA_EN <= '1';
          N_STATE <= decode;
        end if;

      when ex_ls_cnt_check =>
        if (CNT = X"00") then
          ADDR_MUX_SEL <= '1';
          DATA_EN <= '1';
          N_STATE <= decode;
        else
          DATA_EN <= '1';
          ADDR_MUX_SEL <= '1';
          N_STATE <= ex_ls_skip;
        end if;

      when ex_ls_skip =>
        if (DATA_RDATA = X"5B") then
          CNT_INC <= '1';
        elsif (DATA_RDATA = X"5D") then
          CNT_DEC <= '1';
        end if;
        PC_INC <= '1';
        N_STATE <= ex_ls_cnt_check;

      when ex_le =>
        if (DATA_RDATA = X"00") then
          PC_INC <= '1';
          N_STATE <= fetch;
        else
          PC_DEC <= '1';
          CNT_SET <= '1';
          N_STATE <= ex_le_back_l;
        end if;

      when ex_le_back_l =>
        DATA_EN <= '1';
        ADDR_MUX_SEL <= '1';
        N_STATE <= ex_le_back_check;

      when ex_le_back_check =>
        if (DATA_RDATA = X"5B") then
          CNT_DEC <= '1';
        elsif (DATA_RDATA = X"5D") then
          CNT_INC <= '1';
        end if;
        N_STATE <= ex_le_back_cnt_check;

      when ex_le_back_cnt_check =>
        if (CNT = X"00") then
          PC_INC <= '1';
          N_STATE <= fetch;
        else
          PC_DEC <= '1';
          N_STATE <= ex_le_back_l;
        end if;

      -- TMP
      when ex_tmp_load =>
        TMP_ID <= '1';
        ADDR_MUX_SEL <= '1';
        DATA_EN <= '1';
        N_STATE <= decode;

      when ex_tmp_store =>
        WSRC_MUX_SEL <= "01";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        N_STATE <= fetch;

      -- IO
      when ex_print_w =>
        DATA_EN <= '1';
        if (OUT_BUSY = '0') then
          OUT_WE <= '1';
          OUT_DATA <= DATA_RDATA;
          N_STATE <= fetch;
        else
          N_STATE <= ex_print_w;
        end if;

      when ex_print =>
        OUT_WE <= '1';
        OUT_DATA <= DATA_RDATA;
        N_STATE <= fetch;

      when ex_read_w =>
        IN_REQ <= '1';
        if (IN_VLD = '1') then
          DATA_EN <= '1';
          DATA_RDWR <= '0';
          WSRC_MUX_SEL <= "00";
          N_STATE <= fetch;
        else
          N_STATE <= ex_read_w;
        end if;

      when ex_read =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        WSRC_MUX_SEL <= "00";
        N_STATE <= fetch;

      -- HALT
      when halt =>
        DONE <= '1';
        N_STATE <= halt;
    end case;

  end process;

end behavioral;
