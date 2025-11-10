INTRODUCTION

This repository contains the first implementation (v1) of an Energy Hub optimization model developed in GAMS, based on and adapted from the reference paper by A. Barati et al. The original study proposed a multi-energy system integrating Combined Heat and Power (CHP) units, boilers, chillers, storage systems, and renewable sources, with the goal of minimizing operational costs through optimal energy scheduling.

OVERVIEW AND OBJECTIVE

In this first version, the model has been simplified to focus exclusively on the core operational structure of the Energy Hub. Specifically, the water network, hydrogen production, and game-theory multi-agent framework have been removed to isolate and better understand the internal energy and cost dynamics of a single hub. The implemented structure includes CHP, boiler, absorption and electric chillers, electrical and thermal storage, as well as time-varying load profiles and a single electricity price scenario.

The primary objective of this implementation is to minimize total operational costs of the Energy Hub while satisfying the hourly thermal, electrical, and cooling demands. The optimization problem was formulated as a Mixed-Integer Linear Programming (MILP) model solved through GAMS. This version uses a constant natural gas price (0.22 €/kWh) and a single electricity price scenario with a fixed profile over 24 hours. These assumptions allow for analyzing the baseline operational behavior and energy flow interactions between the CHP, absorption chiller, and electric chiller.

RESULTS

The simulation successfully reproduced the expected baseline behavior observed in the reference paper under simplified boundary conditions. Under constant gas and electricity prices, the CHP operated steadily at nominal power, as it was economically convenient compared to grid imports. The absorption chiller covered the entire cooling load, confirming its cost advantage under the given gas-to-heat conversion efficiency. The boiler remained inactive, since the thermal demand was fully satisfied by the CHP waste heat recovery. The storage systems (electrical and thermal) showed stable charge and discharge cycles, though thermal storage required further refinement for cyclic constraints such as the state of charge at hour 1 equal to hour 24. The results align well with the theoretical expectations for a single-day simulation and validate the model’s fundamental structure for cost-optimal operation.

During testing, several limitations and areas for improvement were identified. The thermal storage discharge variable was only active in the final hour due to incomplete boundary constraints, suggesting a need for improved cyclic balance implementation. In the post-processing phase, the absorption chiller initially appeared as the sole contributor to cooling supply, while the electric chiller’s contribution was visible only in the aggregated energy balance. This was later traced to an Excel filtering issue rather than a model error. The CHP output remained constant under the fixed energy prices; to induce modulation, future versions will introduce variable electricity and gas prices or demand response constraints. The model currently simulates only one Energy Hub and a 24-hour horizon; future releases will extend to multi-hub and multi-day frameworks with interconnections and dynamic storage behavior.

FINAL CONSIDERATIONS AND NOTES

This first version serves as the foundation for progressively more complex models. Future developments will include integration of variable energy prices and renewable uncertainty, implementation of game-theoretic optimization for multi-hub interaction, addition of water and hydrogen networks, expansion toward multi-objective optimization combining cost, emissions, and energy efficiency, and coupling with AI-based forecasting models such as LSTM or reinforcement learning for predictive scheduling. These enhancements will evolve the project into a comprehensive Energy Hub AI Optimization Framework, bridging traditional MILP modeling with modern machine learning techniques.

Barati, A., et al. “Multi-objective operation of interconnected multi-energy systems considering power to gas and gas to power systems” 

Author: Ernesto Ceppaluni
MSc Mechanical Engineering for Energy and Environment
Focus: Energy Optimization & AI Modeling
GitHub: @EHubAI
