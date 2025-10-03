# Reflow Oven Controller (UBC ELEC 291)

A microcontroller-based **reflow oven controller** that reads a thermocouple, drives a toaster oven via SSR, and follows a multi-stage reflow profile. Firmware is written in **assembly**, with a **Python** script for live plotting and email alerts.

## Features
- **FSM-driven process**: Idle → Preheat/Soak → Reflow → Cooling
- **K-type thermocouple** + **cold-junction compensation** (LM335)
- **Op-amp amplification** for accurate ADC range
- **SSR control** (PWM duty) for oven power
- **User-set parameters** (soak/reflow temps & times) via push buttons
- **Auto-abort safety**: stops if < 50 °C after 90 s
- **Python monitor**: serial read, real-time plot, stage coloring, email notifications

## How It Works
- **Sensing**: thermocouple + LM335 → op-amp → MCU ADC → temperature conversion.
- **Control**: Timer-driven **FSM** adjusts SSR duty per stage and transitions on
  time/temperature thresholds.
- **UI**: LCD shows stage & setpoints; buttons start/stop and tweak parameters.
- **Safety**: auto-abort if warm-up fails (50 °C not reached in 90 s).
- **PC Tooling**: Python script plots temperature vs. time and sends email alerts
  at stage boundaries.
