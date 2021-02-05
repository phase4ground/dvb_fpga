--
-- DVB IP
--
-- Copyright 2019 by Suoto <andre820@gmail.com>
--
-- This file is part of DVB IP.
--
-- DVB IP is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- DVB IP is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with DVB IP.  If not, see <http://www.gnu.org/licenses/>.

---------------
-- Libraries --
---------------
library	ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library fpga_cores;
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;

library fpga_cores;

------------------------
-- Entity declaration --
------------------------
entity axi_bch_encoder is
  generic (
    TDATA_WIDTH : integer := 8;
    TID_WIDTH   : integer := 0);
  port (
    -- Usual ports
    clk            : in  std_logic;
    rst            : in  std_logic;

    cfg_frame_type : in  frame_type_t;
    cfg_code_rate  : in  code_rate_t;

    -- AXI input
    s_tvalid       : in  std_logic;
    s_tlast        : in  std_logic;
    s_tready       : out std_logic;
    s_tdata        : in  std_logic_vector(TDATA_WIDTH - 1 downto 0);
    s_tid          : in  std_logic_vector(TID_WIDTH - 1 downto 0);

    -- AXI output
    m_tready       : in  std_logic;
    m_tvalid       : out std_logic;
    m_tlast        : out std_logic;
    m_tdata        : out std_logic_vector(TDATA_WIDTH - 1 downto 0);
    m_tid          : out std_logic_vector(TID_WIDTH - 1 downto 0));
end axi_bch_encoder;

architecture axi_bch_encoder of axi_bch_encoder is

  ---------------
  -- Constants --
  ---------------
  -- To count to the max number of words appended to the frame
  constant MAX_WORD_CNT : integer := 192 / TDATA_WIDTH;

  -------------
  -- Signals --
  -------------
  signal frame_type           : frame_type_t;
  signal code_rate            : code_rate_t;

  signal cfg_frame_type_out   : frame_type_t;
  signal cfg_code_rate_out    : code_rate_t;

  signal s_axi_first_word     : std_logic;
  signal tid_reg              : std_logic_vector(TID_WIDTH - 1 downto 0);

  signal axi_delay_tvalid     : std_logic;
  signal axi_delay_tdata      : std_logic_vector(TDATA_WIDTH - 1 downto 0);
  signal axi_delay_tid        : std_logic_vector(TID_WIDTH - 1 downto 0);
  signal axi_delay_tlast      : std_logic;
  signal axi_delay_tready     : std_logic;

  signal axi_delay_data_valid : std_logic;
  signal s_axi_data_valid     : std_logic;
  signal m_axi_data_valid     : std_logic;

  -- Internals to wire output ports
  signal s_tready_i           : std_logic;
  signal m_tvalid_i           : std_logic := '0';
  signal m_tlast_i            : std_logic := '0';

  -- Largest is 192 bits, use a slice when handling polynomials with smaller contexts
  signal crc_word_cnt         : unsigned(numbits(MAX_WORD_CNT) - 1 downto 0);
  signal crc_word_cnt_next    : unsigned(numbits(MAX_WORD_CNT) - 1 downto 0);
  signal crc                  : std_logic_vector(191 downto 0);
  signal crc_srl              : std_logic_vector(191 downto 0);
  signal crc_sample           : std_logic := '0';
  signal crc_sample_delay     : std_logic := '0';

begin

  -------------------
  -- Port mappings --
  -------------------
  -- Delay the incoming data to match the BCH calculation delay so we can switch to
  -- appending without any bubbles
  data_delay_block : block
    signal tdata_agg_in  : std_logic_vector(TID_WIDTH + TDATA_WIDTH downto 0);
    signal tdata_agg_out : std_logic_vector(TID_WIDTH + TDATA_WIDTH downto 0);
  begin
    tdata_agg_in    <= s_tlast & s_tid & s_tdata;

    axi_delay_tdata <= tdata_agg_out(TDATA_WIDTH - 1 downto 0);
    axi_delay_tid   <= tdata_agg_out(TID_WIDTH + TDATA_WIDTH - 1 downto TDATA_WIDTH);
    axi_delay_tlast <= tdata_agg_out(TID_WIDTH + TDATA_WIDTH);

    data_delay_u : entity fpga_cores.axi_stream_delay
      generic map (
        DELAY_CYCLES => 2,
        TDATA_WIDTH  => tdata_agg_in'length)
      port map (
        -- Usual ports
        clk     => clk,
        rst     => rst,

        -- AXI slave input
        s_tvalid => s_tvalid,
        s_tready => s_tready_i,
        s_tdata  => tdata_agg_in,

        -- AXI master output
        m_tvalid => axi_delay_tvalid,
        m_tready => axi_delay_tready,
        m_tdata  => tdata_agg_out);
  end block data_delay_block;

  -- BCH encoders are wrapped with a mux to hide away unrelated stuff. The idea is to keep
  -- the generated CRC codes as similar as possible to how they were generated
  bch_u : entity work.bch_encoder_mux
    generic map (DATA_WIDTH => TDATA_WIDTH)
    port map (
      clk                => clk,
      rst                => rst,

      -- Input data
      cfg_frame_type_in  => frame_type,
      cfg_code_rate_in   => code_rate,

      first_word         => s_axi_first_word,
      wr_en              => s_axi_data_valid,
      wr_data            => s_tdata,

      -- Output data
      cfg_frame_type_out => cfg_frame_type_out,
      cfg_code_rate_out  => cfg_code_rate_out,

      crc_rdy            => open,
      crc                => crc,
      data_out           => open);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  s_axi_data_valid     <= '1' when s_tready_i = '1' and s_tvalid = '1' else '0';
  axi_delay_data_valid <= '1' when axi_delay_tvalid = '1' and axi_delay_tready = '1' else
                          '0';
  m_axi_data_valid     <= '1' when m_tready = '1' and m_tvalid_i = '1' else '0';

  -- Assign internals
  s_tready <= s_tready_i;
  m_tvalid <= m_tvalid_i;
  m_tlast  <= m_tlast_i;

  -- Mux AXI master output to either forward AXI slave (via AXI delay) or to append the
  -- CRC
  axi_delay_tready <= m_tready when crc_word_cnt = 0 else '0';

  m_tvalid_i       <= '0' when rst = '1' else
                      axi_delay_tvalid when crc_word_cnt = 0 else
                      '1';

  m_tdata          <= (others => 'U') when m_tvalid_i = '0' else
                      axi_delay_tdata when crc_word_cnt = 0 else
                      crc_srl(crc_srl'length - 1 downto crc_srl'length - TDATA_WIDTH);

  -- Use axi_delay_tid when passing data through and tid_reg when appending the CRC
  m_tid            <= (others => 'U') when m_tvalid_i = '0' else
                      (tid_reg       and     (m_tid'range => or(crc_word_cnt))) or
                      (axi_delay_tid and not (m_tid'range => or(crc_word_cnt)));

  m_tlast_i        <= '1' when crc_word_cnt = 1 else '0';

  ---------------
  -- Processes --
  ---------------
  process(clk, rst)
  begin
    if rst = '1' then
      crc_word_cnt      <= (others => '0');
      s_axi_first_word  <= '1';

      -- Avoiding unnecessary muxes with rst
      crc_word_cnt_next <= (others => 'U');
      crc_sample        <= 'U';
      crc_sample_delay  <= 'U';
      crc_srl           <= (others => 'U');
    elsif clk'event and clk = '1' then

      crc_sample       <= '0';
      crc_sample_delay <= crc_sample;

      if m_axi_data_valid = '1' then
        if crc_word_cnt = 0 then
          tid_reg <= axi_delay_tid;
        else
          -- Shift the CRC, tdata will be assigned to the MSB
          crc_srl      <= crc_srl(crc_srl'length - TDATA_WIDTH - 1 downto 0)
                          & (TDATA_WIDTH - 1 downto 0 => 'U');
          crc_word_cnt <= crc_word_cnt - 1;
        end if;
      end if;

      if s_axi_data_valid = '1' then
        -- Need to mark the first word so the CRC block can reset the calculation
        s_axi_first_word <= s_tlast;

        -- Need to sync when the CRC output is actually what we want, since we won't
        -- necessarily stop writing data to the CRC calculation block
        if s_tlast = '1' then
          crc_sample      <= '1';
        end if;

        if s_tlast = '1' then
          -- This is mostly static, so division should not be a problem. In any case,
          -- TDATA_WIDTH will always be a power of 2, so this division will map to a shift
          -- operation
          crc_word_cnt_next <= to_unsigned(
                            get_crc_length(frame_type, code_rate) / TDATA_WIDTH,
                            crc_word_cnt'length);
        end if;

      end if;

      -- Time when the CRC is actually completed to sample it
      if crc_sample_delay = '1' then
        -- CRC widths vary, by default BCH encoder mux fills in the LSB. Because we use
        -- the MSB and shift it left, fill in the MSB part of the SRL
        if get_crc_length(cfg_frame_type_out, cfg_code_rate_out) = 128 then
          crc_srl <= crc(127 downto 0) & (63 downto 0 => 'U');
        elsif get_crc_length(cfg_frame_type_out, cfg_code_rate_out) = 160 then
          crc_srl <= crc(159 downto 0) & (31 downto 0 => 'U');
        elsif get_crc_length(cfg_frame_type_out, cfg_code_rate_out) = 168 then
          crc_srl <= crc(167 downto 0) & (23 downto 0 => 'U');
        else
          crc_srl <= crc;
        end if;
      end if;

      -- Handle the completion of AXI delayed data (e.g tlast is high). When that
      -- happens, load the CRC word counter, which will trigger switching to append CRC
      -- mode
      if axi_delay_data_valid = '1' and axi_delay_tlast = '1' then
        crc_word_cnt <= crc_word_cnt_next;
      end if;

    end if;
  end process;

  -- The config ports are valid at the first word of the frame, but we must not rely on
  -- the user keeping it unchanged. Hide this on a block to leave the core code a bit
  -- cleaner
  config_sample_block : block
    signal frame_type_ff : frame_type_t;
    signal code_rate_ff  : code_rate_t;
    signal first_word    : std_logic;
  begin

    frame_type <= cfg_frame_type when first_word = '1' and s_axi_data_valid = '1' else frame_type_ff;
    code_rate  <= cfg_code_rate when first_word = '1' and s_axi_data_valid = '1' else code_rate_ff;

    process(clk)
    begin
      if rising_edge(clk) then
        if s_axi_data_valid = '1' then
          first_word <= s_tlast;

          -- Sample the BCH code used on the first word
          if first_word = '1' then
            frame_type_ff <= cfg_frame_type;
            code_rate_ff  <= cfg_code_rate;
          end if;

        end if;

        if rst = '1' then
          first_word    <= '1';
        end if;

      end if;
    end process;
  end block config_sample_block;

end axi_bch_encoder;
