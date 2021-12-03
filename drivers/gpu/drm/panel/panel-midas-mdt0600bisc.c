// SPDX-License-Identifier: GPL-2.0
/*
 * Panel driver for Midas Display's MDT0600BISC panel with DSI adapter board
 * Copyright (C) 2021 Pete Marshall, Syntax Music Ltd.
 * 
 * Derived from panel-ilitek-ili9881c.c by Maxime Ripard
 * Copyright (C) 2017-2018, Bootlin
 */

#include <linux/backlight.h>
#include <linux/delay.h>
#include <linux/device.h>
#include <linux/err.h>
#include <linux/errno.h>
#include <linux/fb.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/of_device.h>

#include <linux/gpio/consumer.h>
#include <linux/regulator/consumer.h>

#include <drm/drm_mipi_dsi.h>
#include <drm/drm_modes.h>
#include <drm/drm_panel.h>

#include <video/mipi_display.h>

enum ili9881c_op {
	ILI9881C_SWITCH_PAGE,
	ILI9881C_COMMAND,
};

struct ili9881c_instr {
	enum ili9881c_op	op;

	union arg {
		struct cmd {
			u8	cmd;
			u8	data;
		} cmd;
		u8	page;
	} arg;
};

struct ili9881c_desc {
	const struct ili9881c_instr *init;
	const size_t init_length;
	const struct drm_display_mode *mode;
};

struct mdt0600bisc {
	struct drm_panel	panel;
	struct mipi_dsi_device	*dsi;
	const struct ili9881c_desc	*desc;

	struct gpio_desc	*dimctrl;
	struct gpio_desc	*reset;
};

#define ILI9881C_SWITCH_PAGE_INSTR(_page)	\
	{					\
		.op = ILI9881C_SWITCH_PAGE,	\
		.arg = {			\
			.page = (_page),	\
		},				\
	}

#define ILI9881C_COMMAND_INSTR(_cmd, _data)		\
	{						\
		.op = ILI9881C_COMMAND,		\
		.arg = {				\
			.cmd = {			\
				.cmd = (_cmd),		\
				.data = (_data),	\
			},				\
		},					\
	}

static const struct ili9881c_instr mdt0600bisc_init[] = {
	ILI9881C_SWITCH_PAGE_INSTR(3),
	ILI9881C_COMMAND_INSTR(0x01, 0x00),
	ILI9881C_COMMAND_INSTR(0x02, 0x00),
	ILI9881C_COMMAND_INSTR(0x03, 0x53),
	ILI9881C_COMMAND_INSTR(0x04, 0x13),
	ILI9881C_COMMAND_INSTR(0x05, 0x00),
	ILI9881C_COMMAND_INSTR(0x06, 0x04),
	ILI9881C_COMMAND_INSTR(0x07, 0x00),
	ILI9881C_COMMAND_INSTR(0x08, 0x00),
	ILI9881C_COMMAND_INSTR(0x09, 0x21),
	ILI9881C_COMMAND_INSTR(0x0a, 0x21),
	ILI9881C_COMMAND_INSTR(0x0b, 0x00),
	ILI9881C_COMMAND_INSTR(0x0c, 0x01),
	ILI9881C_COMMAND_INSTR(0x0d, 0x00),
	ILI9881C_COMMAND_INSTR(0x0e, 0x00),
	ILI9881C_COMMAND_INSTR(0x0f, 0x21),
	ILI9881C_COMMAND_INSTR(0x10, 0x21),
	ILI9881C_COMMAND_INSTR(0x11, 0x00),
	ILI9881C_COMMAND_INSTR(0x12, 0x00),
	ILI9881C_COMMAND_INSTR(0x13, 0x00),
	ILI9881C_COMMAND_INSTR(0x14, 0x00),
	ILI9881C_COMMAND_INSTR(0x15, 0x00),
	ILI9881C_COMMAND_INSTR(0x16, 0x00),
	ILI9881C_COMMAND_INSTR(0x17, 0x00),
	ILI9881C_COMMAND_INSTR(0x18, 0x00),
	ILI9881C_COMMAND_INSTR(0x19, 0x00),
	ILI9881C_COMMAND_INSTR(0x1a, 0x00),
	ILI9881C_COMMAND_INSTR(0x1b, 0x00),
	ILI9881C_COMMAND_INSTR(0x1c, 0x00),
	ILI9881C_COMMAND_INSTR(0x1d, 0x00),
	ILI9881C_COMMAND_INSTR(0x1e, 0x40),
	ILI9881C_COMMAND_INSTR(0x1f, 0x80),
	ILI9881C_COMMAND_INSTR(0x20, 0x02),
	ILI9881C_COMMAND_INSTR(0x21, 0x03),
	ILI9881C_COMMAND_INSTR(0x22, 0x00),
	ILI9881C_COMMAND_INSTR(0x23, 0x00),
	ILI9881C_COMMAND_INSTR(0x24, 0x00),
	ILI9881C_COMMAND_INSTR(0x25, 0x00),
	ILI9881C_COMMAND_INSTR(0x26, 0x00),
	ILI9881C_COMMAND_INSTR(0x27, 0x00),
	ILI9881C_COMMAND_INSTR(0x28, 0x33),
	ILI9881C_COMMAND_INSTR(0x29, 0x03),
	ILI9881C_COMMAND_INSTR(0x2a, 0x00),
	ILI9881C_COMMAND_INSTR(0x2b, 0x00),
	ILI9881C_COMMAND_INSTR(0x2c, 0x00),
	ILI9881C_COMMAND_INSTR(0x2d, 0x00),
	ILI9881C_COMMAND_INSTR(0x2e, 0x00),
	ILI9881C_COMMAND_INSTR(0x2f, 0x00),
	ILI9881C_COMMAND_INSTR(0x30, 0x00),
	ILI9881C_COMMAND_INSTR(0x31, 0x00),
	ILI9881C_COMMAND_INSTR(0x32, 0x00),
	ILI9881C_COMMAND_INSTR(0x33, 0x00),
	ILI9881C_COMMAND_INSTR(0x34, 0x04),
	ILI9881C_COMMAND_INSTR(0x35, 0x00),
	ILI9881C_COMMAND_INSTR(0x36, 0x00),
	ILI9881C_COMMAND_INSTR(0x37, 0x00),
	ILI9881C_COMMAND_INSTR(0x38, 0x3C),
	ILI9881C_COMMAND_INSTR(0x39, 0x00),
	ILI9881C_COMMAND_INSTR(0x3a, 0x40),
	ILI9881C_COMMAND_INSTR(0x3b, 0x40),
	ILI9881C_COMMAND_INSTR(0x3c, 0x00),
	ILI9881C_COMMAND_INSTR(0x3d, 0x00),
	ILI9881C_COMMAND_INSTR(0x3e, 0x00),
	ILI9881C_COMMAND_INSTR(0x3f, 0x00),
	ILI9881C_COMMAND_INSTR(0x40, 0x00),
	ILI9881C_COMMAND_INSTR(0x41, 0x00),
	ILI9881C_COMMAND_INSTR(0x42, 0x00),
	ILI9881C_COMMAND_INSTR(0x43, 0x00),
	ILI9881C_COMMAND_INSTR(0x44, 0x00),
	ILI9881C_COMMAND_INSTR(0x50, 0x01),
	ILI9881C_COMMAND_INSTR(0x51, 0x23),
	ILI9881C_COMMAND_INSTR(0x52, 0x45),
	ILI9881C_COMMAND_INSTR(0x53, 0x67),
	ILI9881C_COMMAND_INSTR(0x54, 0x89),
	ILI9881C_COMMAND_INSTR(0x55, 0xab),
	ILI9881C_COMMAND_INSTR(0x56, 0x01),
	ILI9881C_COMMAND_INSTR(0x57, 0x23),
	ILI9881C_COMMAND_INSTR(0x58, 0x45),
	ILI9881C_COMMAND_INSTR(0x59, 0x67),
	ILI9881C_COMMAND_INSTR(0x5a, 0x89),
	ILI9881C_COMMAND_INSTR(0x5b, 0xab),
	ILI9881C_COMMAND_INSTR(0x5c, 0xcd),
	ILI9881C_COMMAND_INSTR(0x5d, 0xef),
	ILI9881C_COMMAND_INSTR(0x5e, 0x11),
	ILI9881C_COMMAND_INSTR(0x5f, 0x01),
	ILI9881C_COMMAND_INSTR(0x60, 0x00),
	ILI9881C_COMMAND_INSTR(0x61, 0x15),
	ILI9881C_COMMAND_INSTR(0x62, 0x14),
	ILI9881C_COMMAND_INSTR(0x63, 0x0C),
	ILI9881C_COMMAND_INSTR(0x64, 0x0D),
	ILI9881C_COMMAND_INSTR(0x65, 0x0E),
	ILI9881C_COMMAND_INSTR(0x66, 0x0F),
	ILI9881C_COMMAND_INSTR(0x67, 0x06),
	ILI9881C_COMMAND_INSTR(0x68, 0x02),
	ILI9881C_COMMAND_INSTR(0x69, 0x02),
	ILI9881C_COMMAND_INSTR(0x6a, 0x02),
	ILI9881C_COMMAND_INSTR(0x6b, 0x02),
	ILI9881C_COMMAND_INSTR(0x6c, 0x02),
	ILI9881C_COMMAND_INSTR(0x6d, 0x02),
	ILI9881C_COMMAND_INSTR(0x6e, 0x08),
	ILI9881C_COMMAND_INSTR(0x6f, 0x02),
	ILI9881C_COMMAND_INSTR(0x70, 0x02),
	ILI9881C_COMMAND_INSTR(0x71, 0x02),
	ILI9881C_COMMAND_INSTR(0x72, 0x02),
	ILI9881C_COMMAND_INSTR(0x73, 0x02),
	ILI9881C_COMMAND_INSTR(0x74, 0x02),
	ILI9881C_COMMAND_INSTR(0x75, 0x01),
	ILI9881C_COMMAND_INSTR(0x76, 0x00),
	ILI9881C_COMMAND_INSTR(0x77, 0x15),
	ILI9881C_COMMAND_INSTR(0x78, 0x14),
	ILI9881C_COMMAND_INSTR(0x79, 0x0C),
	ILI9881C_COMMAND_INSTR(0x7a, 0x0F),
	ILI9881C_COMMAND_INSTR(0x7b, 0x0E),
	ILI9881C_COMMAND_INSTR(0x7c, 0x0F),
	ILI9881C_COMMAND_INSTR(0x7d, 0x08),
	ILI9881C_COMMAND_INSTR(0x7e, 0x02),
	ILI9881C_COMMAND_INSTR(0x7f, 0x02),
	ILI9881C_COMMAND_INSTR(0x80, 0x02),
	ILI9881C_COMMAND_INSTR(0x81, 0x02),
	ILI9881C_COMMAND_INSTR(0x82, 0x02),
	ILI9881C_COMMAND_INSTR(0x83, 0x02),
	ILI9881C_COMMAND_INSTR(0x84, 0x06),
	ILI9881C_COMMAND_INSTR(0x85, 0x02),
	ILI9881C_COMMAND_INSTR(0x86, 0x02),
	ILI9881C_COMMAND_INSTR(0x87, 0x02),
	ILI9881C_COMMAND_INSTR(0x88, 0x02),
	ILI9881C_COMMAND_INSTR(0x89, 0x02),
	ILI9881C_COMMAND_INSTR(0x8A, 0x02),
	ILI9881C_SWITCH_PAGE_INSTR(4),
	ILI9881C_COMMAND_INSTR(0x6C, 0x15),
	ILI9881C_COMMAND_INSTR(0x6E, 0x2B),	// POWER SET2 VGH +15V
	ILI9881C_COMMAND_INSTR(0x6F, 0x35), // POWER SET3 33
	ILI9881C_COMMAND_INSTR(0x35, 0x1F),
	ILI9881C_COMMAND_INSTR(0x33, 0x14),	// Blanking frame GND
	ILI9881C_COMMAND_INSTR(0x3A, 0x24), // PS_EN=0 power saving 94
	ILI9881C_COMMAND_INSTR(0x8D, 0x1A), // POWER SET 4	VGL -10.8v-11
	ILI9881C_COMMAND_INSTR(0x87, 0xBA),
	ILI9881C_COMMAND_INSTR(0x26, 0x76),
	ILI9881C_COMMAND_INSTR(0xB2, 0xD1),
	ILI9881C_COMMAND_INSTR(0xB5, 0x06),
	ILI9881C_SWITCH_PAGE_INSTR(1),
	ILI9881C_COMMAND_INSTR(0x22, 0x0A), // BGR SS GS 09-180£¬0A-0
	ILI9881C_COMMAND_INSTR(0x31, 0x00), // Display inversion control oo-column inversion;01-1dot,02-2dot
	ILI9881C_COMMAND_INSTR(0x53, 0x8C), // vci=3.3v
	ILI9881C_COMMAND_INSTR(0x55, 0x8C), // VCOM vci=3.3v
	ILI9881C_COMMAND_INSTR(0x50, 0XC7), // VCOM_R VREG1OUT	positive Gamma//VREG1  5.1V
	ILI9881C_COMMAND_INSTR(0x51, 0xC4), // VREG2 -5.1V VREG2OUT negative Gamma
	ILI9881C_COMMAND_INSTR(0x60, 0x1C),
//	ILI9881C_COMMAND_INSTR(0x61, 0x00),
	ILI9881C_COMMAND_INSTR(0x62, 0x00), // EQ
	ILI9881C_COMMAND_INSTR(0x63, 0x00), // PC
	ILI9881C_COMMAND_INSTR(0x2E, 0xF0),	// 1440 GATE NL SEL
	ILI9881C_COMMAND_INSTR(0xA0, 0x00),
	ILI9881C_COMMAND_INSTR(0xA1, 0x26),
	ILI9881C_COMMAND_INSTR(0xA2, 0x34),
	ILI9881C_COMMAND_INSTR(0xA3, 0x14),
	ILI9881C_COMMAND_INSTR(0xA4, 0x17),
	ILI9881C_COMMAND_INSTR(0xA5, 0x2A),
	ILI9881C_COMMAND_INSTR(0xA6, 0x1D),
	ILI9881C_COMMAND_INSTR(0xA7, 0x1E),
	ILI9881C_COMMAND_INSTR(0xA8, 0x8B),
	ILI9881C_COMMAND_INSTR(0xA9, 0x1B),
	ILI9881C_COMMAND_INSTR(0xAA, 0x27),
	ILI9881C_COMMAND_INSTR(0xAB, 0x72),
	ILI9881C_COMMAND_INSTR(0xAC, 0x1D),
	ILI9881C_COMMAND_INSTR(0xAD, 0x1D),
	ILI9881C_COMMAND_INSTR(0xAE, 0x51),
	ILI9881C_COMMAND_INSTR(0xAF, 0x26),
	ILI9881C_COMMAND_INSTR(0xB0, 0x2B),
	ILI9881C_COMMAND_INSTR(0xB1, 0x49),
	ILI9881C_COMMAND_INSTR(0xB2, 0x58),
	ILI9881C_COMMAND_INSTR(0xB3, 0x26),
	ILI9881C_COMMAND_INSTR(0xC0, 0x00),	//  GAMMA Negative
	ILI9881C_COMMAND_INSTR(0xC1, 0x26),
	ILI9881C_COMMAND_INSTR(0xC2, 0x34),
	ILI9881C_COMMAND_INSTR(0xC3, 0x14),
	ILI9881C_COMMAND_INSTR(0xC4, 0x17),
	ILI9881C_COMMAND_INSTR(0xC5, 0x2A),
	ILI9881C_COMMAND_INSTR(0xC6, 0x1D),
	ILI9881C_COMMAND_INSTR(0xC7, 0x1E),
	ILI9881C_COMMAND_INSTR(0xC8, 0x8B),
	ILI9881C_COMMAND_INSTR(0xC9, 0x1B),
	ILI9881C_COMMAND_INSTR(0xCA, 0x27),
	ILI9881C_COMMAND_INSTR(0xCB, 0x72),
	ILI9881C_COMMAND_INSTR(0xCC, 0x1D),
	ILI9881C_COMMAND_INSTR(0xCD, 0x1D),
	ILI9881C_COMMAND_INSTR(0xCE, 0x51),
	ILI9881C_COMMAND_INSTR(0xCF, 0x26),
	ILI9881C_COMMAND_INSTR(0xD0, 0x26),
	ILI9881C_COMMAND_INSTR(0xD1, 0x49),
	ILI9881C_COMMAND_INSTR(0xD2, 0x58),
	ILI9881C_COMMAND_INSTR(0xD3, 0x26),
	ILI9881C_SWITCH_PAGE_INSTR(0),
	ILI9881C_COMMAND_INSTR(0x35, 0x00),
	ILI9881C_COMMAND_INSTR(0x11, 0x00),
//	Delay(120);
	ILI9881C_COMMAND_INSTR(0x29, 0x00),
//	Delay(25);
};

static inline struct mdt0600bisc *panel_to_mdt0600bisc(struct drm_panel *panel)
{
	return container_of(panel, struct mdt0600bisc, panel);
}

/*
 * The panel seems to accept some private DCS commands that map
 * directly to registers.
 *
 * It is organised by page, with each page having its own set of
 * registers, and the first page looks like it's holding the standard
 * DCS commands.
 *
 * So before any attempt at sending a command or data, we have to be
 * sure if we're in the right page or not.
 */
static int ili9881c_switch_page(struct mdt0600bisc *ctx, u8 page)
{
	u8 buf[4] = { 0xff, 0x98, 0x81, page };
	int ret;

	ret = mipi_dsi_dcs_write_buffer(ctx->dsi, buf, sizeof(buf));
	if (ret < 0)
		return ret;

	return 0;
}

static int ili9881c_send_cmd_data(struct mdt0600bisc *ctx, u8 cmd, u8 data)
{
	u8 buf[2] = { cmd, data };
	int ret;

	ret = mipi_dsi_dcs_write_buffer(ctx->dsi, buf, sizeof(buf));
	if (ret < 0)
		return ret;

	return 0;
}


// Backlight driver is a Texas Instrument's TPS61165
// Here we enter 1 wire Easy Scale digital backlight dimming mode
// See page 12 of datasheet 
static int mdt0600bisc_init_backlight(struct gpio_desc *dimctrl)
{
	// 0. reset the TPS61165
	gpiod_set_value(dimctrl, 0);
	msleep(3);	// should be 2.5ms (at least?) but can't use udelay(2500) due to __bad_udelay on compilation

	// 1. pull high to start 1 wire detection window
	gpiod_set_value(dimctrl, 1);
	udelay(100);

	// 2. after ES detection delay (100us), drive low for ES detection time (260us)
	gpiod_set_value(dimctrl, 0);
	udelay(260);

	// 3. dimctrl has to be low for > ES detection time before ES detection window (1ms) expires
	// 	  ES detection window starts from first dimctrl low to high transition (step 1)

	// the TPS61165 enters 1 wire mode once the above 3 condiitions are met
	// ES comms can start before the detection window expires
	// Once dimming mode is programmed, it cannot be changed without another start up

	// static high for subsequent dimming programming (see 10.2.1.2.3 in datasheet)
	gpiod_set_value(dimctrl, 1);

	return 0;
}

static int mdt0600bisc_bl_send_byte(struct gpio_desc *dimctrl, u8 data)
{
	const int t1 = 100;		// us
	const int t2 = t1 * 2;	// us
	int mask = 0x80;

	// t_start
	gpiod_set_value(dimctrl, 1);
	udelay(2);

	while (mask)
	{
		int level = data & mask;
		if (level)
		{
			gpiod_set_value(dimctrl, 0);
			udelay(t1);
			gpiod_set_value(dimctrl, 1);
			udelay(t2);
		}
		else
		{
			gpiod_set_value(dimctrl, 0);
			udelay(t2);
			gpiod_set_value(dimctrl, 1);
			udelay(t1);
		}
		mask >>= 1;
	}

	// t_eos
	gpiod_set_value(dimctrl, 0);
	udelay(2);

	// static high
	gpiod_set_value(dimctrl, 1);

	return 0;
}

static int mdt0600bisc_set_bl_brightness(struct gpio_desc *dimctrl, u8 brightness)
{
	mdt0600bisc_bl_send_byte(dimctrl, 0x72);		// device address byte
	mdt0600bisc_bl_send_byte(dimctrl, brightness);	// data byte (LSB 5 bits = DATA, MSB = Request for acknowlege = 0)
	return 0;
}

static int mdt0600bisc_prepare(struct drm_panel *panel)
{
	struct mdt0600bisc *ctx = panel_to_mdt0600bisc(panel);
	unsigned int i;
	int ret;

	// power on here, removed regulator from original driver
	msleep(5);

	/* And reset it */
	gpiod_set_value(ctx->reset, 1);
	msleep(20);

	gpiod_set_value(ctx->reset, 0);
	msleep(20);

	// set the backlight to half brightness via the 1 wire "EasyScale" protocol
	mdt0600bisc_init_backlight(ctx->dimctrl);
	mdt0600bisc_set_bl_brightness(ctx->dimctrl, 8);

	for (i = 0; i < ctx->desc->init_length; i++) {
		const struct ili9881c_instr *instr = &ctx->desc->init[i];

		if (instr->op == ILI9881C_SWITCH_PAGE)
			ret = ili9881c_switch_page(ctx, instr->arg.page);
		else if (instr->op == ILI9881C_COMMAND)
			ret = ili9881c_send_cmd_data(ctx, instr->arg.cmd.cmd,
						      instr->arg.cmd.data);

		if (ret)
			return ret;
	}

	ret = ili9881c_switch_page(ctx, 0);
	if (ret)
		return ret;

	ret = mipi_dsi_dcs_set_tear_on(ctx->dsi, MIPI_DSI_DCS_TEAR_MODE_VBLANK);
	if (ret)
		return ret;

	ret = mipi_dsi_dcs_exit_sleep_mode(ctx->dsi);
	if (ret)
		return ret;

	return 0;
}

static int mdt0600bisc_enable(struct drm_panel *panel)
{
	struct mdt0600bisc *ctx = panel_to_mdt0600bisc(panel);

	msleep(120);

	mipi_dsi_dcs_set_display_on(ctx->dsi);

	return 0;
}

static int mdt0600bisc_disable(struct drm_panel *panel)
{
	struct mdt0600bisc *ctx = panel_to_mdt0600bisc(panel);

	return mipi_dsi_dcs_set_display_off(ctx->dsi);
}

static int mdt0600bisc_unprepare(struct drm_panel *panel)
{
	struct mdt0600bisc *ctx = panel_to_mdt0600bisc(panel);

	mipi_dsi_dcs_enter_sleep_mode(ctx->dsi);
	gpiod_set_value(ctx->reset, 1);

	return 0;
}

static const struct drm_display_mode mdt0600bisc_default_mode = {
	.clock		= 76400,

	.hdisplay	= 720,
	.hsync_start	= 720 + 20,
	.hsync_end	= 720 + 20 + 4,
	.htotal		= 720 + 20 + 4 + 120,

	.vdisplay	= 1440,
	.vsync_start	= 1440 + 10,
	.vsync_end	= 1440 + 10 + 4,
	.vtotal		= 1440 + 10 + 4 + 20,

	.width_mm	= 68,
	.height_mm	= 136,
};

static int mdt0600bisc_get_modes(struct drm_panel *panel,
			      struct drm_connector *connector)
{
	struct mdt0600bisc *ctx = panel_to_mdt0600bisc(panel);
	struct drm_display_mode *mode;

	mode = drm_mode_duplicate(connector->dev, ctx->desc->mode);
	if (!mode) {
		dev_err(&ctx->dsi->dev, "failed to add mode %ux%ux@%u\n",
			ctx->desc->mode->hdisplay,
			ctx->desc->mode->vdisplay,
			drm_mode_vrefresh(ctx->desc->mode));
		return -ENOMEM;
	}

	drm_mode_set_name(mode);

	mode->type = DRM_MODE_TYPE_DRIVER | DRM_MODE_TYPE_PREFERRED;
	drm_mode_probed_add(connector, mode);

	connector->display_info.width_mm = mode->width_mm;
	connector->display_info.height_mm = mode->height_mm;

	return 1;
}

static const struct drm_panel_funcs mdt0600bisc_funcs = {
	.prepare	= mdt0600bisc_prepare,
	.unprepare	= mdt0600bisc_unprepare,
	.enable		= mdt0600bisc_enable,
	.disable	= mdt0600bisc_disable,
	.get_modes	= mdt0600bisc_get_modes,
};

static int mdt0600bisc_bl_update_status(struct backlight_device *bl)
{
	struct mipi_dsi_device *dsi = bl_get_data(bl);
	struct mdt0600bisc *ctx = mipi_dsi_get_drvdata(dsi);
	u16 brightness = bl->props.brightness;

	// what's this for?
	if (bl->props.power != FB_BLANK_UNBLANK ||
	    bl->props.fb_blank != FB_BLANK_UNBLANK ||
	    bl->props.state & (BL_CORE_SUSPENDED | BL_CORE_FBBLANK))
		brightness = 0;

	if (brightness < 1)
		brightness = 1;

	if (brightness > 31)
		brightness = 31;

	mdt0600bisc_set_bl_brightness(ctx->dimctrl, brightness);

	bl->props.brightness = brightness;

	return 0;
}

static int mdt0600bisc_bl_get_brightness(struct backlight_device *bl)
{
	return bl->props.brightness;
}

static const struct backlight_ops mdt0600bisc_bl_ops = {
	.update_status = mdt0600bisc_bl_update_status,
	.get_brightness = mdt0600bisc_bl_get_brightness,
};

static struct backlight_device *
mdt0600bisc_create_backlight(struct mipi_dsi_device *dsi)
{
	struct device *dev = &dsi->dev;
	const struct backlight_properties props = {
		.type = BACKLIGHT_PLATFORM,
		.brightness = 8,
		.max_brightness = 31,
	};

	return devm_backlight_device_register(dev, dev_name(dev), dev, dsi,
					      &mdt0600bisc_bl_ops, &props);
}

static int mdt0600bisc_dsi_probe(struct mipi_dsi_device *dsi)
{
	struct mdt0600bisc *ctx;

	ctx = devm_kzalloc(&dsi->dev, sizeof(*ctx), GFP_KERNEL);
	if (!ctx)
		return -ENOMEM;
	mipi_dsi_set_drvdata(dsi, ctx);
	ctx->dsi = dsi;
	ctx->desc = of_device_get_match_data(&dsi->dev);

	drm_panel_init(&ctx->panel, &dsi->dev, &mdt0600bisc_funcs,
		       DRM_MODE_CONNECTOR_DSI);

	ctx->dimctrl = devm_gpiod_get(&dsi->dev, "dimctrl", GPIOD_OUT_HIGH);
	if (IS_ERR(ctx->dimctrl)) {
		dev_err(&dsi->dev, "Couldn't get our dimctrl GPIO\n");
		return PTR_ERR(ctx->dimctrl);
	}

	ctx->reset = devm_gpiod_get(&dsi->dev, "reset", GPIOD_OUT_LOW);
	if (IS_ERR(ctx->reset)) {
		dev_err(&dsi->dev, "Couldn't get our reset GPIO\n");
		return PTR_ERR(ctx->reset);
	}

	ctx->panel.backlight = mdt0600bisc_create_backlight(dsi);
	if (IS_ERR(ctx->panel.backlight)) {
		dev_err(&dsi->dev, "Failed to create backlight\n");
		return PTR_ERR(ctx->panel.backlight);
	}

	drm_panel_add(&ctx->panel);

	dsi->mode_flags = MIPI_DSI_MODE_VIDEO_SYNC_PULSE | MIPI_DSI_MODE_VIDEO;
	dsi->format = MIPI_DSI_FMT_RGB888;
	dsi->lanes = 4;

	return mipi_dsi_attach(dsi);
}

static int mdt0600bisc_dsi_remove(struct mipi_dsi_device *dsi)
{
	struct mdt0600bisc *ctx = mipi_dsi_get_drvdata(dsi);

	mipi_dsi_detach(dsi);
	drm_panel_remove(&ctx->panel);

	return 0;
}

static const struct ili9881c_desc mdt0600bisc_desc = {
	.init = mdt0600bisc_init,
	.init_length = ARRAY_SIZE(mdt0600bisc_init),
	.mode = &mdt0600bisc_default_mode,
};

static const struct of_device_id mdt0600bisc_of_match[] = {
	{ .compatible = "midasdisplays,mdt0600bisc", .data = &mdt0600bisc_desc },
	{ }
};
MODULE_DEVICE_TABLE(of, mdt0600bisc_of_match);

static struct mipi_dsi_driver mdt0600bisc_dsi_driver = {
	.probe		= mdt0600bisc_dsi_probe,
	.remove		= mdt0600bisc_dsi_remove,
	.driver = {
		.name		= "mdt0600bisc-dsi",
		.of_match_table	= mdt0600bisc_of_match,
	},
};
module_mipi_dsi_driver(mdt0600bisc_dsi_driver);

MODULE_AUTHOR("Pete Marshall <petemarshall303@gmail.com>");
MODULE_DESCRIPTION("Midas MDT0600BISC Panel Driver");
MODULE_LICENSE("GPL v2");

