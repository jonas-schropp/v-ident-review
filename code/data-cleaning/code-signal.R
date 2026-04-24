# Signal type and acquisition

code_modality <- function(signal) {
  
  signal %>%
    mutate(
      ECG = as.integer(grepl("ECG", modality)),
      EDA = as.integer(grepl("EDA", modality) | grepl("GSR", modality)),
      PPG = as.integer(grepl("PPG", modality)),
      SCG = as.integer(grepl("SCG", modality)),
      VHF = as.integer(grepl("VHF", modality)),
      microphone = as.integer(grepl("microphone", modality)),
      multimodal = if_else(ECG + EDA + PPG + SCG + VHF + microphone > 1, 1, 0),
      modality = gsub("GSR", "EDA", modality)
    ) 
  
}

code_vital_parameter <- function(signal) {
  
  signal %>%
    mutate(
      HR = pmax (ECG , SCG, VHF),
      GSR = EDA,
      BVP = PPG,
      breathing = pmax( microphone, VHF )
    ) %>%
    select(
      -`vital parameter`
    )
  
}

code_users <- function(signal) {
  
  signal %>%
    mutate(
      `user characteristics` = gsub( '?', 'not reported', `user characteristics` ),
      `user characteristics` = gsub( 'none', 'not reported', `user characteristics` ),
      `user characteristics` = gsub( 'not provided', 'not reported', `user characteristics` ),
      `age and gender reported` = if_else(
        `user characteristics` %in% c(
          '12 healthy male Chinese  subjects, 18-35 y.o.',
          '14m 6f, 25-32y, Taiwan',
          '20 healthy Chinese individuals, 18-58y, male',
          '24m 29f, 22.9±3.1y',
          '26m 40f; 16-79y',
          '42m 47f; 18-48y',
          '7f 6m, 20-50y, no cv diseases',
          'Bosch employees and students (Germany), 7m 8f 30.60 ± 9.59 y',
          'healthy teachers, 16m 12f, 25-35J',
          'm&f, 20-60y'
        ), 1, 0
      )
    )
  
}

code_device <- function(signal) {
  
  signal %>% 
    mutate(
      `device location` = case_when(
        device %in% c(
          'Apple watch', 'ASUS VivoWatch BP', 'Empatica E4', 'Fitbit Charge HR',
          'Fitbit Charge HR+Evistr Digital microphone Recorder',
          'Fitbit Ionic', 'Fitbit Ionic + microphone', 
          'Huawei Watch G2', 'Maxim wristband', 'Microsoft Band 2', 
          'own device (left wrist)', 'own device (right wrist)', 
          'Mio Fuse, Fitbit, Microsoft Band', 'own device (wrist band)',
          'own device (wrist bands)', 'own device (wrist)', 
          'Samsung Gear S', 'Samsung Gear S2', 'wrist worn (dorsal)'
        ) ~ "wrist",
        device %in% c(
          'Hexoskin Hx1', 'Hexoskin HX1', 'Hexoskin Proshirt',
          'Hexoskin Proshirt / HeartIn Fit shirt', 
          'own device (chest strap)', 'own device (heart)',
          'own device (sternum)', 'Savvy (chest-worn)'
        ) ~ "chest/torso",
        device %in% c(
          'own device (left carotid artery)'
        ) ~ "neck"
      ),
      device = case_when(
        device %in% c(
          'own device (left wrist)', 'own device (right wrist)', 
          'own device (wrist band)', 'wrist worn (dorsal)',
          'own device (wrist bands)', 'own device (wrist)',
          'own device (chest strap)', 'own device (heart)',
          'own device (sternum)', 'own device (left carotid artery)'
        ) ~ 'own device',
        device == 'Savvy (chest-worn)' ~ 'Savvy',
        device == 'Hexoskin HX1' ~ 'Hexoskin Hx1',
        .default = device
      )
    )
  
}


code_sampling <- function(signal) {
  
  signal %>%
    mutate(
      `sampling frequency` = if_else(
        `sampling frequency` %in% c(
          '?', 'not reported', 'one sample per minute'
        ), 'not reported', `sampling frequency` 
      ),
      `sampling frequency` = if_else(
        `sampling frequency` == 'one sample per minute + 44.1 kHz', 
        'not reported, 44.1 kHz', `sampling frequency` 
      ),
      `sampling frequency` = if_else(
        `sampling frequency` == '16 Hz, 32Hz,  64 Hz', 
        '16 Hz, 32 Hz, 64 Hz', `sampling frequency` 
      ),
      `sampling frequency` = if_else(
        `sampling frequency` == '256 Hz & 512 Hz', 
        '256 Hz, 512 Hz', `sampling frequency` 
      ),
      `sampling frequency` = if_else(
        `sampling frequency` == '300hz', 
        '300 Hz', `sampling frequency` 
      ),
      `sampling frequency` = if_else(
        `sampling frequency` == '4 & 34 Hz', 
        '4 Hz, 34 Hz', `sampling frequency` 
      )
    )
  
}


code_channels <- function(signal) {
  
  signal %>%
    mutate(
      `number of channels / electrodes` = case_when(
        `number of channels / electrodes` %in% c(
          '?', 'not described'
        ) ~ 'not reported',
        `number of channels / electrodes` %in% c(
          'likely one (green)', 'one (green light)',
          'one (green)', 'one, green LED with wavelength 515 nm', '1?'
        ) ~ 'one channel (green)',
        .default = `number of channels / electrodes`
      )
    )
}


code_duration <- function(signal) {
  
  signal %>%
    mutate(
      `acquisition time` = ordered(
        case_when(
          `acquisition time` %in% c(
            "3 min", "5min", "20x10s (users); 10x10s (intruders)",
            "2+? min", "2+1 min", "2+3 min", "2+2 min", "2min+3min", 
            "5 min", "30s+60s+60s+60s+60s+30s"
          ) ~ 1,
          `acquisition time` %in% c(
            "15 min", "4x5 min", "6x300 s", "3x300 s", "10min", 
            "41x30s", "5d x 5s x >5min", "minimum 15 minutes", 
            "3 min + minimum 12 minutes", "2 x 3 min + minimum 12 minutes",
            "40 x 15 seconds", "5 x 3 min", "at least 15 min",
            "variable, ca. 5 minutes per session (two individuals double)"
          ) ~ 2,
          `acquisition time` %in% c(
            ">30 min x 2 lessons x 2 days", "20+40+20+20min", 
            "2 x minimum 15 minutes", "1 hour"
          ) ~ 3,
          `acquisition time` %in% c(
            ">240 min", ">240 min x 2 days", ">240 min x 3 days",
            "5 days (except nights)", "5d x 10+20min", "6 hours",
            "4 hours", "ca. 60 hours"
          ) ~ 4,
        ),
        levels = 1:4,
        labels = c("up to 5 min", "5-30 min", "30-120 min", "over 120 min")
      )
    ) 
  
}

code_users <- function(signal) {
  
  signal %>%
    mutate(
      `number of individuals` = as.integer(
        if_else(
          `number of individuals` == "2 users, 38 intruders",
          "2", `number of individuals`
        )
      )
    )
}


code_conditions <- function(signal) {
  
  signal %>%
    mutate(
      conditions = factor(
        case_when(
          signal %in% c(
            "during driving task", "resting", "working", "sitting", 
            "standing", "walking", "post-exercise", "idle+idle", "rest",
            "sitting in car", "Resting", "Walking", "Standing", "Uphill walking",                                                                                                                                                                                                                                                                                
            "3 min of standing", "3 min of sitting", "3 min of typical walking"
          ) ~ 1L,
          signal %in% c(
            "resting, walking, standing and uphill walking",
            "baseline, lecture, examination, recovery",
            "resting, working", "resting, working, resting",
            "different situations and stress levels", 
            "idle+typing 1", "idle+touchpad", "idle+typing 2", 
            "far-wrist activities (i.e., moving\r\nthe forearms) + sitting",                                                                                                                                                                                                                                 
            "far-wrist activities (i.e., moving\r\nthe forearms)  + sitting",                                                                                                                                                                                                                                
            "near-wrist activities (i.e., grabbing\r\nup a cup and drinking water) repeatedly  + sitting",                                                                                                                                                                                                   
            "near-wrist activities (i.e., grabbing\r\nup a cup and drinking water) repeatedly + sitting",
            "7 continuous movements with the arms + sitting",
            "10min resting, 20min exercise",
            "3 minutes lite and easy walking activities, after that normal routine activities",
            "3 min of standing + 6 min uncontrolled",                                                                                                                                                                                                                                                        
            "3 min each of standing, sitting and walking + 6 min uncontrolled",                                                                                                                                                                                                                              
            "3 min each of sitting and walking + 3 min standing?",
            "T1: walking or running on treadmill: 1–2 km/h (30 s), 6–8 km/h (1 min), 12–15 km/h (1 min), 6–8 km/h (1 min), 12–15 km/h (1 min), and 1–2 km/h (30 s); T2: forearm/upper arm exercises (e.g. shake hands, stretch, push, running, jump, and push-ups). T3: intense arm movements (e.g. boxing)",
            "walking or running on treadmill: 1–2 km/h (30 s), 6–8 km/h (1 min), 12–15 km/h (1 min), 6–8 km/h (1 min), 12–15 km/h (1 min), and 1–2 km/h (30 s)"
          ) ~ 2L,
          signal %in% c(
            "regular daily activities", "everyday activities", 
            "daily routine activity", 
            "daily living; activities, e.g., sitting, walking and resting, and sleep",
            "real life scenario (recording without a predefined task to make it as natural as possible)",
            "everyday conditions"
          ) ~ 3L,
          .default = NA_integer_
        ),
        levels = 1:3,
        labels = c("one condition", "multiple conditions", "everyday activities")
      )
    )
}


code_permanence <- function(signal) {
  
  signal %>%
    mutate(
      permanence = factor(
        case_when(
          `time interval` %in% c(
            "one session", "few minutes", "2h", "3h", "1 hour"
          ) ~ 1L,
          `time interval` %in% c(
            "3 consecutive days", "2 consecutive days", "one day in between",
            "different days for users, same day for intruders", 
            "5 consecutive days", "20 min between sessions, 5 days",
            "5 days"
          ) ~ 2L,
          `time interval` %in% c(
            "8 weeks", "30 days"
          ) ~ 3L,
          .default = NA_integer_
        ),
        levels = 1:3,
        labels = c("low (one day)", "medium (multiple days)", "high (> one week)")
      )
    ) %>%
    select(-`time interval`)
}


