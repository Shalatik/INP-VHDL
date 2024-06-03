-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2021 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Simona Ceskova xcesko00
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
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WREN  : out std_logic;                    -- cteni z pameti (DATA_WREN='0') / zapis do pameti (DATA_WREN='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WREN musi byt '0'
   OUT_WREN : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
 -- zde dopiste potrebne deklarace signalu

--deklarace vsech signalu
	-- PC
	--protoze CODE_ADDR : out std_logic_vector(11 downto 0)
	signal PC_register : std_logic_vector(11 downto 0);
	signal PC_increase : std_logic;
	signal PC_decrease : std_logic;
	
	--CNT
	signal CNT_register : std_logic_vector(7 downto 0);
	signal CNT_increase : std_logic;
	signal CNT_decrease : std_logic;
	
	--PTR
	--protoze DATA_ADDR  : out std_logic_vector(9 downto 0)
	signal PTR_register : std_logic_vector(9 downto 0);
	signal PTR_increase : std_logic;
	signal PTR_decrease : std_logic;
	
	--MX
	signal MX_selector: std_logic_vector(1 downto 0) := "00";
	--rozhoduji podle toho, jestli je na selectoru 00, 01, 10, 11
	
--konec deklarace vsech signalu

--deklarace stavu FSM
	type FSM_states is (
		FSM_start,
		FSM_fetch,
		FSM_decode, --stav jednotky DC, ktera dekoduje signal na prikazy v brainfcku
		
		--inkrementace/dekrementace pointeru, neboli posouvani v registrech
		FSM_pointer_increase,
		FSM_pointer_decrease,
		
		--inkrementace hodnoty aktualni bunky je dvoutaktova
		FSM_value_increase,
		--v druhem taktu pristupuji do RAM pameti, kde prepisuju data 
		FSM_value_increase_RAM,
		
		--dekrementace hodnoty aktualni bunky je dvoutaktova
		FSM_value_decrease,
		--v druhem taktu pristupuji do RAM pameti, kde prepisuju data 
		FSM_value_decrease_RAM,

		--while loop cyklus [ zacatek
		FSM_loop_start_0,
		--dalsi potrebne takty, aby se hodnoty stihly nacist
		FSM_loop_start_1,
		FSM_loop_start_2,
		--mezistavy:
		FSM_loop_start_3,
		FSM_loop_start_4,

		--while loop cyklus ] ukonceni
		FSM_loop_end_0,
		--dalsi potrebne takty, aby se hodnoty stihly nacist
		FSM_loop_end_1,
		FSM_loop_end_2,
		FSM_loop_end_3,
		--mezistavy:
		FSM_loop_end_4,
		FSM_loop_end_5,
		FSM_loop_end_6,

		--break ukonci prave provadenou smycku while
		FSM_break_0,
		FSM_break_1,
		FSM_break_2,
		
		FSM_null, --return
		
		--vypise na vystup hodnotu aktualni bunky
		FSM_write,
		--v druhem taktu pristupuji do RAM pameti, protoze zapisuji data
		FSM_write_RAM,
		
		--nacte hodnotu a ulozi ji do aktualni bunky
		FSM_read,
		
		--ostatni
		FSM_others
	);
	
	--aktivni stav, ten ktery ted zpracovavam
	signal active_state : FSM_states;
	--dalsi stav, na ktery povede aktivni stav
	signal next_state : FSM_states;
	
--konec deklarace stavu FSM


begin
 -- zde dopiste vlastni VHDL kod

 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly.
 


--------------------------PC
	-- PC funguuje jako programovy citac, ukazuje do pameti programu
	PC_process: process(RESET, CLK, PC_increase, PC_decrease)
	begin
		if RESET = '1' then --RESET ma prednost a pokud == 1 tak se vse resetuje a nezalezi na tom co je potom
			PC_register <= (others => '0');
		elsif RESET = '0' and rising_edge(CLK) then
			if PC_increase = '1' then
				PC_register <= PC_register + 1; -- ++PC
			elsif PC_decrease = '1' then
				PC_register <= PC_register - 1; -- --PC
			end if;
		end if;
	end process;
--------------------------PC

--------------------------CNT 
	--CNT k urceni zacatku a konce [ ] while loop prikazu
	CNT_process: process(RESET, CLK, CNT_increase, CNT_decrease)
  begin
    if RESET = '1' then
			CNT_register <= (others => '0');
		elsif RESET = '0' and rising_edge(CLK) then
			if CNT_increase = '1' then
				CNT_register <= CNT_register + 1; -- ++CNT
			elsif CNT_decrease = '1' then
				CNT_register <= CNT_register - 1; -- --CNT
			end if;
		end if;
	end process;
--------------------------CNT

--------------------------PTR 
	--PTR funguje jako ukazatel do pameti dat
	PTR_process: process(RESET, CLK, PTR_increase, PTR_decrease)
  begin
    if RESET = '1' then
			PTR_register <= (others => '0');
		elsif RESET = '0' and rising_edge(CLK) then
			if PTR_increase = '1' then 
				PTR_register <= PTR_register + 1; -- ++PTR
			elsif PTR_decrease = '1' then 
				PTR_register <= PTR_register - 1; -- --PTR
      end if;
    end if;
	end process;
--------------------------PTR
	
--------------------------MX 
	--multiplexor ridi hodnotu zapisovanou do pameti RAM
	MX_process: process(RESET, CLK, MX_selector, IN_DATA)
  begin
			if MX_selector = "00" then
				DATA_WDATA <= IN_DATA; 	--nacteni dat
			elsif MX_selector = "01" then
				DATA_WDATA <= DATA_RDATA - "00000001"; -- -1
			elsif MX_selector = "10" then
				DATA_WDATA <= DATA_RDATA + "00000001"; -- +1
			elsif MX_selector = "11" then
				DATA_WDATA <= X"00"; -- X"00" == null
      end if;
	end process;
--------------------------MX

	--deklarace cest
	CODE_ADDR <= PC_register;
	DATA_ADDR <= PTR_register;
	--deklarace cest

--------------------------FMS	
	FSM_process: process(RESET, CLK, EN)
  begin
    if RESET = '1' then
			--pokud je zapnuty RESET, tak se vrati zpatky do startu celho procesu - jde zpatky na start
			active_state <= FSM_start;
		elsif EN = '1' and rising_edge(CLK) then
			--pokud neni RESET zapnuty, tak muze pokracovat dal - prechazi na dalsi stav
      active_state <= next_state;
    end if;
	end process;
--------------------------FMS
	
	FSM_main_process: process(RESET, CLK)
	begin
	
		--inicializovani vsech signalu za zacatku na pocatecni hodnoty
		DATA_WREN <= '0';
		IN_REQ <= '0';
		CODE_EN <= '1'; --musi byt 1 aby mohlo dojit k dalsimu behu programu
		PC_increase <= '0';
		PC_decrease <= '0';
		CNT_increase <= '0';
		CNT_decrease <= '0';
		PTR_increase <= '0';
		PTR_decrease <= '0';
		MX_selector <= "00";
		DATA_EN <= '0';
		OUT_WREN <= '0';
		--konec inicializaceaa
		
		--hledani typu signalu jaky prisel
		if active_state = FSM_start then
			next_state <= FSM_fetch;
		elsif active_state = FSM_fetch then
			--CODE_EN povoluje spusteni dalsich kroku
			CODE_EN <= '1';
			next_state <= FSM_decode;
		elsif active_state = FSM_decode then
		
			---------------------------------------dekodovani prikazu
			case CODE_DATA is
				when X"3E" =>
					next_state <= FSM_pointer_increase;
				when X"3C" =>
					next_state <= FSM_pointer_decrease;
				when X"2B" =>
					next_state <= FSM_value_increase;
				when X"2D" =>
					next_state <= FSM_value_decrease;
				when X"5B" =>
					next_state <= FSM_loop_start_0;
				when X"5D" =>
					next_state <= FSM_loop_end_0;
				when X"2E" =>
					next_state <= FSM_write;
				when X"2C" =>
					next_state <= FSM_read;
				when X"7E" =>
					next_state <= FSM_break_0;
				when X"00" =>			
					next_state <= FSM_null;
				when others =>
					next_state <= FSM_others;
			end case;
			--------------------------------------konec dekodovani prikazu
	--pokud aktivni stav je dekodovany prikaz, tak se zacne provadet jeho ucel
		
		--inkrementace hodnoty ukazatele
		elsif active_state = FSM_pointer_increase then
			PTR_increase <= '1';
			PC_increase <= '1';
			next_state <= FSM_fetch;
		--dekrementace hodnoty ukazatele
		elsif active_state = FSM_pointer_decrease then
			PTR_decrease <= '1';
			PC_increase <= '1';
			next_state <= FSM_fetch;
			
	----------------------------------------------------------
	
		--inkrementace hodnoty aktualni bunky
		elsif active_state = FSM_value_increase then
			DATA_EN <= '1';
			DATA_WREN <= '0';
			next_state <= FSM_value_increase_RAM;
		elsif active_state = FSM_value_increase_RAM then
			DATA_EN <= '1';
			DATA_WREN <= '1'; --povoleni k zapisovani
			MX_selector <= "10"; --MX zvysi hodnotu aktualni bunky
			PC_increase <= '1';
			next_state <= FSM_fetch;
		
		--dekrementace hodnoty aktualni bunky
		elsif active_state = FSM_value_decrease then
			DATA_EN <= '1';
			DATA_WREN <= '0';
			next_state <= FSM_value_decrease_RAM;
		elsif active_state = FSM_value_decrease_RAM then
			DATA_EN <= '1';
			DATA_WREN <= '1'; --povoleni k zapisovani
			MX_selector <= "01"; --MX zmensi hodnotu aktualni bunky
			PC_increase <= '1';
			next_state <= FSM_fetch;
			
	----------------------------------------------------------
		
		--vytiskni hodnotu aktualni bunky
		elsif active_state = FSM_write then
			DATA_WREN <= '0';
			DATA_EN <= '1';
			next_state <= FSM_write_RAM;
			--dalsi takty, RAM protoze v tomto momente pracuje s RAM pameti
		elsif active_state = FSM_write_RAM then
			if OUT_BUSY = '1' then
				DATA_WREN <= '0';
				DATA_EN <= '1';
				next_state <= FSM_write_RAM;
			elsif OUT_BUSY = '0' then
				OUT_WREN <= '1'; --povoleni vypisovani
				PC_increase <= '1';
				OUT_DATA <= DATA_RDATA; --vypis dat ven
				next_state <= FSM_fetch;
			end if;
			
	----------------------------------------------------------
		
		elsif active_state = FSM_read then
			next_state <= FSM_fetch;
		
	----------------------------------------------------------
		
		-- [ WHILE START
		--je-li hodnota odpovídající buòky nulová, skoè za odpovídající pøíkaz ] jinak pokraèuj následujícím znakem
		elsif active_state = FSM_loop_start_0 then
			DATA_EN <= '1';
			PC_increase <= '1';
			DATA_WREN <= '0';
			next_state <= FSM_loop_start_3;
		elsif active_state = FSM_loop_start_3 then -- MEZISTAV, aby se stihly nacist hodnoty
			CNT_increase <= '0'; --vynulovani aby nedochazelo k nechtenemu pricitani
			CNT_decrease <= '0';
			next_state <= FSM_loop_start_1;
		elsif active_state = FSM_loop_start_1 then
			if DATA_RDATA /= "00000000" then -- DATA_RDATA != 0
				next_state <= FSM_fetch;
			else
				CNT_increase <= '1';
				next_state <= FSM_loop_start_4;
			end if;
		elsif active_state = FSM_loop_start_4 then -- MEZISTAV
			CNT_increase <= '0';
			CNT_decrease <= '0';
			next_state <= FSM_loop_start_2;
		elsif active_state = FSM_loop_start_2 then
			if CNT_register /= "00000000" then -- CNT != 0
				CODE_EN <= '1';
				if CODE_DATA = X"5B" then -- if CODE_DATA == '['
					CNT_increase <= '1';
				elsif CODE_DATA = X"5D" then -- if CODE_DATA == ']'
					CNT_decrease <= '1';
				end if;
				PC_increase <= '1';
				next_state <= FSM_loop_start_3; --vraceni se, aby se vykonal loop
			else
				next_state <= FSM_fetch;
			end if;
		
		----------------------------------------------------------
		
		-- ] WHILE END
		--je-li hodnota odpovídající buòky nenulová, skoè za odpovídající pøíkaz [ jinak pokraèuj následujícím znakem
		elsif active_state = FSM_loop_end_0 then
			DATA_EN <= '1';
			DATA_WREN <= '0';
			next_state <= FSM_loop_end_4;
		elsif active_state = FSM_loop_end_4 then -- MEZISTAV, aby se stihly nacist hodnoty
			CNT_increase <= '0'; --vynulovani aby nedochazelo k nechtenemu pricitani
			CNT_decrease <= '0';
			next_state <= FSM_loop_end_1;
		elsif active_state = FSM_loop_end_1 then
			if DATA_RDATA = "00000000" then -- DATA_RDATA == 0
				PC_increase <= '1'; 
				next_state <= FSM_fetch;
			else
				CNT_increase <= '1';
				PC_decrease <= '1';
				CODE_EN <= '1';
				next_state <= FSM_loop_end_5;
			end if;
		elsif active_state = FSM_loop_end_5 then -- MEZISTAV
			CNT_increase <= '0';
			CNT_decrease <= '0';
			DATA_EN <= '0';
			next_state <= FSM_loop_end_2;	
		elsif active_state = FSM_loop_end_2 then
		if CODE_DATA = X"5D" then -- if CODE_DATA == ']'
			CNT_increase <= '1';
			elsif CODE_DATA = X"5B" then -- if CODE_DATA == '['
				CNT_decrease <= '1';
			end if;
			next_state <= FSM_loop_end_6;
		elsif active_state = FSM_loop_end_6 then -- MEZISTAV
			CNT_increase <= '0';
			CNT_decrease <= '0';
			DATA_EN <= '0';
			next_state <= FSM_loop_end_3;		
		elsif active_state = FSM_loop_end_3 then
			if CNT_register = "00000000" then -- CNT = 0
				PC_increase <= '1';
				next_state <= FSM_fetch;
			else
				PC_decrease <= '1';
				next_state <= FSM_loop_end_5;
			end if;
			CODE_EN <= '1';
			
	----------------------------------------------------------
		
		--ukonci prave provadenou smycku while
		elsif active_state = FSM_break_0 then
			CNT_increase <= '1';
			PC_increase <= '1';
			DATA_EN <= '1';
			next_state <= FSM_break_1;
			
		elsif active_state = FSM_break_1 then
			if CNT_register = "00000000" then --CNT == 0, pokud neni zadny loop v kterym je break
				next_state <= FSM_fetch;
			else
				CODE_EN <= '1'; --povoli dalsi kroky
				next_state <= FSM_break_2;
			end if;
			
		elsif active_state = FSM_break_2 then
			if CODE_DATA = X"5B" then --if CODE_DATA == '['
				CNT_increase <= '1';
			elsif CODE_DATA = X"5D" then --if CODE_DATA == ']'
				CNT_decrease <= '1';
			end if;
			PC_increase <= '1';
			next_state <= FSM_break_1;
			
	----------------------------------------------------------

		--zastavi vykonavani programu (RETURN)
		elsif active_state = FSM_null then
			next_state <= FSM_null;
			
	----------------------------------------------------------

		--others pro ostatni pripady
		elsif active_state = FSM_others then
			PC_increase <= '1';
			next_state <= FSM_fetch;

		end if;
	
	end process;
end behavioral;
 
