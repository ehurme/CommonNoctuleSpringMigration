# Scripts for "Bats Surf Storm Fronts During Spring Migration"

This repository contains R scripts and associated data processing workflows used for the analysis in the paper **"Bats Surf Storm Fronts During Spring Migration."** Each script performs specific tasks related to data preprocessing, analysis, and visualization of bat migration and environmental data.

https://www.science.org/doi/10.1126/science.ade7441
---

## **Scripts Overview**

### **Data Preprocessing and Analysis**

#### **1. 1_download_data.R**
**Purpose:**
- Download and preprocess movement and environmental data for noctule bats from Movebank and associated metadata.

**Key Outputs:**
- `spring_swiss_migration_steps_full.robj` (cleaned dataset)

---

#### **2. 2_burst_flying_threshold.R**
**Purpose:**
- Calculate and visualize thresholds for flying behavior based on VeDBA and ground speed metrics.

**Key Outputs:**
- `vpm_speed_threshold.robj` (thresholds)
- Supplementary Figure S1

---

#### **3. 3_migration_steps_env_days.R**
**Purpose:**
- Extract and process environmental data at flight timestamps for multiple days surrounding migration.

**Key Outputs:**
- `migration_steps_env_days.robj` (processed environmental data)

---

#### **4. 4_regularize_tracks.R**
**Purpose:**
- Regularize movement tracks to consistent daily intervals and calculate daily metrics.

**Key Outputs:**
- `n_day.robj` (daily summarized dataset)

---

#### **5. 5_behavior_thresholds.R**
**Purpose:**
- Identify behavioral thresholds for migration-related activities using Gaussian mixture models.

**Key Outputs:**
- `behavior_thresholds.robj` (behavioral thresholds)

---

#### **6. 6_migration_metrics.R**
**Purpose:**
- Estimate departure dates and migration path metrics.

**Key Outputs:**
- `migration_metrics.robj` (migration metrics)
- Table S7

---

#### **7. 7_stopover_duration.R**
**Purpose:**
- Estimate stopover durations using first passage time analysis.

**Key Outputs:**
- `stopover.robj` (stopover durations)
- Supplementary Figure S4

---

#### **8. 8_envdata_hourly.R**
**Purpose:**
- Extract hourly environmental data for migration analysis.

**Key Outputs:**
- `n_day_env.robj` (hourly environmental data)

---

#### **9. 9_interpolate_migration.R**
**Purpose:**
- Interpolate migration paths and calculate environmental metrics along the routes.

**Key Outputs:**
- `migration_interp.robj` (interpolated migration paths)

---

#### **10. 10_clean_data.R**
**Purpose:**
- Clean and prepare data for modeling migration patterns.

**Key Outputs:**
- `clean_data.robj` (cleaned data for modeling)

---

#### **11. 11_env_models.R**
**Purpose:**
- Fit and analyze environmental models for migration behavior and energetics.

**Key Outputs:**
- Tables S6, S9, S10
- `migration_model.robj`

---

#### **12. 12_departure_conditions.R**
**Purpose:**
- Extract environmental data at capture locations during sunset for tagging periods.

**Key Outputs:**
- Supplementary Figure S5

---

#### **13. 13_markov_transition.R**
**Purpose:**
- Fit a Markov model to analyze behavioral state transitions.

**Key Outputs:**
- Table S2 (Transition Probability Matrix)

---

#### **14. 14_summarize_movement.R**
**Purpose:**
- Summarize movement metrics and analyze behavioral states.

**Key Outputs:**
- Supplementary Figure S3

---

### **Figures Scripts**

#### **Figure1.R**
**Purpose:**
- Generate a composite map showing migration behaviors over time and space.

**Key Outputs:**
- Figure 1

---

#### **Figure2.R**
**Purpose:**
- Analyze wind conditions and visualize environmental metrics with ridge plots.

**Key Outputs:**
- Figure 2

---

#### **Figure2_windmap.R**
**Purpose:**
- Generate wind vector maps for migratory paths.

**Key Outputs:**
- Supplementary wind vector maps

---

#### **Figure3.R**
**Purpose:**
- Examine migration timing and environmental conditions like temperature and precipitation.

**Key Outputs:**
- Figure 3

---

#### **Figure3_pressuremap.R**
**Purpose:**
- Visualize pressure maps during migration events.

**Key Outputs:**
- Supplementary pressure maps

---

#### **Figure4.R**
**Purpose:**
- Explore migration timing and costs associated with distance and wind conditions.

**Key Outputs:**
- Figure 4

---

### **Usage Instructions**
1. Clone this repository.
2. Run each script sequentially (Scripts 1 to 14), followed by the figure generation scripts.
3. Ensure all dependencies (packages and data files) are installed and available.
4. Adjust file paths as needed to match your local directory structure.

---
