# ICD code file for someone to check over to make sure that the codes we've
# chosen actually represent what we want the codes to represent

# CVD-related codes -------------------------------------------------------

# we have separate lists because we might want to do analyses breaking down
# CVD-related hospitalizations into multiple categories

library(tidyverse)

# here, aiming to capture any hospitalization for an MI
# myocardial infarction
mi_icd_9 <- c(
  '41000',  # Acute myocardial infarction of anterolateral wall, episode of care unspecified
  '41010',  # Acute myocardial infarction of other anterior wall, episode of care unspecified
  '41020',  # Acute myocardial infarction of inferolateral wall, episode of care unspecified
  '41030',  # Acute myocardial infarction of inferoposterior wall, episode of care unspecified
  '41040',  # Acute myocardial infarction of other inferior wall, episode of care unspecified
  '41050',  # Acute myocardial infarction of other lateral wall, episode of care unspecified
  '41060',  # True posterior wall infarction, episode of care unspecified
  '41070',  # Subendocardial infarction, episode of care unspecified
  '41080',  # Acute myocardial infarction of other specified sites, episode of care unspecified
  '41090',  # Acute myocardial infarction, unspecified site, episode of care unspecified
  '41001',  # Acute myocardial infarction of anterolateral wall, initial episode of care
  '41011',  # Acute myocardial infarction of other anterior wall, initial episode of care
  '41021',  # Acute myocardial infarction of inferolateral wall, initial episode of care
  '41031',  # Acute myocardial infarction of inferoposterior wall, initial episode of care
  '41041',  # Acute myocardial infarction of other inferior wall, initial episode of care
  '41051',  # Acute myocardial infarction of other lateral wall, initial episode of care
  '41061',  # True posterior wall infarction, initial episode of care
  '41071',  # Subendocardial infarction, initial episode of care
  '41081',  # Acute myocardial infarction of other specified sites, initial episode of care
  '41091'   # Acute myocardial infarction, unspecified site, initial episode of care
)

mi_icd_10 <- c(
  'I21.09', # Acute transmural myocardial infarction of unspecified site
  'I21.09', # STEMI involving other anterior wall
  'I21.19', # STEMI involving inferior wall
  'I21.29', # STEMI involving posterior wall
  'I21.29', # STEMI involving other inferior wall
  'I21.29', # STEMI involving other lateral wall
  'I21.29', # STEMI involving true posterior wall
  'I21.4',  # Non-ST elevation (NSTEMI) myocardial infarction
  'I21.29', # STEMI involving other specified sites
  'I21.3',  # STEMI of unspecified site
  'I21.09', # STEMI involving anterolateral wall, initial episode
  'I21.09', # STEMI involving other anterior wall, initial episode
  'I21.19', # STEMI involving inferolateral wall, initial episode
  'I21.29', # STEMI involving inferoposterior wall, initial episode
  'I21.29', # STEMI involving other inferior wall, initial episode
  'I21.29', # STEMI involving other lateral wall, initial episode
  'I21.29', # STEMI involving true posterior wall, initial episode
  'I21.4',  # NSTEMI, initial episode
  'I21.29', # STEMI involving other specified sites, initial episode
  'I21.3'   # STEMI of unspecified site, initial episode
)

# here, aiming to capture any ischemic stroke hosp
istroke_icd_9 <-
  c('436',      # Acute but ill-defined cerebrovascular disease
    '43301',    # Occlusion and stenosis of basilar artery with cerebral infarction
    '43311',    # Occlusion and stenosis of carotid artery with cerebral infarction
    '43321',    # Occlusion and stenosis of vertebral artery with cerebral infarction
    '43331',    # Occlusion and stenosis of multiple and bilateral arteries with cerebral infarction
    '43381',    # Occlusion and stenosis of other specified precerebral arteries with cerebral infarction
    '43391',    # Occlusion and stenosis of unspecified precerebral artery with cerebral infarction
    '43401',    # Cerebral thrombosis with cerebral infarction
    '43411',    # Cerebral embolism with cerebral infarction
    '43491')    # Cerebral artery occlusion, unspecified with cerebral infarction


# here, aiming to capture any a fib hosp
afib_icd_9 <- c(
  '42731',  # Atrial fibrillation (chronic)
  '42732')  # Atrial flutter

afib_icd_10 <- c(
  'I480',    # Paroxysmal atrial fibrillation
  'I481',    # Persistent atrial fibrillation
  'I4811',   # Longstanding persistent atrial fibrillation
  'I4819',   # Other persistent atrial fibrillation
  'I482',    # Chronic atrial fibrillation
  'I4820',   # Unspecified chronic atrial fibrillation
  'I4821',   # Permanent atrial fibrillation
  'I483',    # Typical atrial flutter
  'I484',    # Atypical atrial flutter
  'I4891',   # Other specified atrial fibrillation
  'I4892')   # Other specified atrial flutter

# here, aiming to capture any hypertension, even if it was preexisting
hyp_icd_9 <-
  c("4011",   # Essential hypertension, benign
    "4019",   # Essential hypertension, unspecified
    "40210",  # Hypertensive heart disease without heart failure, benign
    "40290",  # Hypertensive heart disease without heart failure, unspecified
    "40511",  # Hypertensive heart and renal disease, benign
    "40519",  # Hypertensive heart and renal disease, unspecified
    "40591",  # Hypertensive heart and renal disease, malignant
    "40599")  # Secondary hypertension, unspecified

hyp_icd_10 <- c(
  "I10",      # Essential (primary) hypertension
  "I169",     # Hypertension with complications
  "I119",     # Hypertension with heart failure
  "1110",     # Hypertensive heart disease
  "1129",     # Hypertensive renal disease
  "1120",     # Hypertensive heart and renal disease
  "11310",    # Pulmonary hypertension, primary
  "1130",     # Secondary pulmonary hypertension
  "11311",    # Chronic thromboembolic pulmonary hypertension
  "1132",     # Other pulmonary hypertension
  "1150",     # Hypertensive retinopathy
  "1158")     # Hypertensive encephalopathy

# any ischemic stroke
istroke_icd_9 <-
  c('43301',  # Occlusion and stenosis of the carotid artery with cerebral infarction
    '43311',  # Occlusion and stenosis of the vertebral artery with cerebral infarction
    '43321',  # Occlusion and stenosis of the basilar artery with cerebral infarction
    '43331',  # Occlusion and stenosis of multiple and bilateral precerebral arteries with cerebral infarction
    '43381',  # Occlusion and stenosis of other specified precerebral arteries with cerebral infarction
    '43391',  # Occlusion and stenosis of unspecified precerebral artery with cerebral infarction
    '43401',  # Cerebral thrombosis with cerebral infarction
    '43411',  # Cerebral embolism with cerebral infarction
    '43491',  # Cerebral artery occlusion, unspecified, with cerebral infarction
    '436')    # Acute, but ill-defined, cerebrovascular disease (often used to describe unspecified stroke)

istroke_icd_10 <-
  c('I6300',  # Cerebral infarction due to thrombosis of precerebral arteries
    'I63011',  # Cerebral infarction due to thrombosis of middle cerebral artery, right side
    'I63012',  # Cerebral infarction due to thrombosis of middle cerebral artery, left side
    'I63013',  # Cerebral infarction due to thrombosis of middle cerebral artery, bilateral
    'I63019',  # Cerebral infarction due to thrombosis of middle cerebral artery, unspecified side
    'I6302',   # Cerebral infarction due to thrombosis of multiple and bilateral precerebral arteries
    'I63031',  # Cerebral infarction due to embolism of middle cerebral artery, right side
    'I63032',  # Cerebral infarction due to embolism of middle cerebral artery, left side
    'I63033',  # Cerebral infarction due to embolism of middle cerebral artery, bilateral
    'I63039',  # Cerebral infarction due to embolism of middle cerebral artery, unspecified side
    'I6309',   # Cerebral infarction due to thrombosis of precerebral artery, unspecified
    'I6310',   # Cerebral infarction due to thrombosis of cerebral arteries, unspecified
    'I63111',  # Cerebral infarction due to thrombosis of right anterior cerebral artery
    'I63112',  # Cerebral infarction due to thrombosis of left anterior cerebral artery
    'I63113',  # Cerebral infarction due to thrombosis of bilateral anterior cerebral arteries
    'I63119',  # Cerebral infarction due to thrombosis of anterior cerebral artery, unspecified side
    'I6312',   # Cerebral infarction due to thrombosis of multiple and bilateral cerebral arteries
    'I63131',  # Cerebral infarction due to embolism of right anterior cerebral artery
    'I63132',  # Cerebral infarction due to embolism of left anterior cerebral artery
    'I63133',  # Cerebral infarction due to embolism of bilateral anterior cerebral arteries
    'I63139',  # Cerebral infarction due to embolism of anterior cerebral artery, unspecified side
    'I6319',   # Cerebral infarction due to thrombosis or embolism of unspecified cerebral artery
    'I6320',   # Cerebral infarction due to thrombosis of other specified cerebral arteries
    'I63211',  # Cerebral infarction due to embolism of right posterior cerebral artery
    'I63212',  # Cerebral infarction due to embolism of left posterior cerebral artery
    'I63213',  # Cerebral infarction due to embolism of bilateral posterior cerebral arteries
    'I63219',  # Cerebral infarction due to embolism of posterior cerebral artery, unspecified side
    'I6322',   # Cerebral infarction due to thrombosis of multiple and bilateral posterior cerebral arteries
    'I63231',  # Cerebral infarction due to embolism of right posterior cerebral artery
    'I63232',  # Cerebral infarction due to embolism of left posterior cerebral artery
    'I63233',  # Cerebral infarction due to embolism of bilateral posterior cerebral arteries
    'I63239',  # Cerebral infarction due to embolism of posterior cerebral artery, unspecified side
    'I6329',   # Cerebral infarction due to thrombosis of unspecified posterior cerebral artery
    'I6330',   # Cerebral infarction due to thrombosis of other specified cerebral arteries
    'I63311',  # Cerebral infarction due to embolism of right middle cerebral artery
    'I63312',  # Cerebral infarction due to embolism of left middle cerebral artery
    'I63313',  # Cerebral infarction due to embolism of bilateral middle cerebral arteries
    'I63319',  # Cerebral infarction due to embolism of middle cerebral artery, unspecified side
    'I63321',  # Cerebral infarction due to embolism of right vertebral artery
    'I63322',  # Cerebral infarction due to embolism of left vertebral artery
    'I63323',  # Cerebral infarction due to embolism of bilateral vertebral arteries
    'I63329',  # Cerebral infarction due to embolism of vertebral artery, unspecified side
    'I63331',  # Cerebral infarction due to embolism of right basilar artery
    'I63332',  # Cerebral infarction due to embolism of left basilar artery
    'I63333',  # Cerebral infarction due to embolism of bilateral basilar arteries
    'I63339',  # Cerebral infarction due to embolism of basilar artery, unspecified side
    'I63341',  # Cerebral infarction due to embolism of right carotid artery
    'I63342',  # Cerebral infarction due to embolism of left carotid artery
    'I63343',  # Cerebral infarction due to embolism of bilateral carotid arteries
    'I63349',  # Cerebral infarction due to embolism of carotid artery, unspecified side
    'I6339',   # Cerebral infarction due to embolism of other specified cerebral arteries
    'I6340',   # Cerebral infarction due to thrombosis of cerebral arteries, unspecified
    'I63411',  # Cerebral infarction due to embolism of right anterior cerebral artery
    'I63412',  # Cerebral infarction due to embolism of left anterior cerebral artery
    'I63413',  # Cerebral infarction due to embolism of bilateral anterior cerebral arteries
    'I63419',  # Cerebral infarction due to embolism of anterior cerebral artery, unspecified side
    'I63421',  # Cerebral infarction due to embolism of right middle cerebral artery
    'I63422',  # Cerebral infarction due to embolism of left middle cerebral artery
    'I63423',  # Cerebral infarction due to embolism of bilateral middle cerebral arteries
    'I63429',  # Cerebral infarction due to embolism of middle cerebral artery, unspecified side
    'I63431',  # Cerebral infarction due to embolism of right posterior cerebral artery
    'I63432',  # Cerebral infarction due to embolism of left posterior cerebral artery
    'I63433',  # Cerebral infarction due to embolism of bilateral posterior cerebral arteries
    'I63439',  # Cerebral infarction due to embolism of posterior cerebral artery, unspecified side
    'I63441',  # Cerebral infarction due to embolism of right vertebral artery
    'I63442',  # Cerebral infarction due to embolism of left vertebral artery
    'I63443',  # Cerebral infarction due to embolism of bilateral vertebral arteries
    'I63449',  # Cerebral infarction due to embolism of vertebral artery, unspecified side
    'I6349',   # Cerebral infarction due to embolism of other specified cerebral arteries
    'I6350',   # Cerebral infarction due to thrombosis of cerebral arteries, unspecified
    'I63511',  # Cerebral infarction due to embolism of right basilar artery
    'I63512',  # Cerebral infarction due to embolism of left basilar artery
    'I63513',  # Cerebral infarction due to embolism of bilateral basilar arteries
    'I63519',  # Cerebral infarction due to embolism of basilar artery, unspecified side
    'I63521',  # Cerebral infarction due to embolism of right carotid artery
    'I63522',  # Cerebral infarction due to embolism of left carotid artery
    'I63523',  # Cerebral infarction due to embolism of bilateral carotid arteries
    'I63529',  # Cerebral infarction due to embolism of carotid artery, unspecified side
    'I63531',  # Cerebral infarction due to embolism of right middle cerebral artery
    'I63532',  # Cerebral infarction due to embolism of left middle cerebral artery
    'I63533',  # Cerebral infarction due to embolism of bilateral middle cerebral arteries
    'I63539',  # Cerebral infarction due to embolism of middle cerebral artery, unspecified side
    'I63541',  # Cerebral infarction due to embolism of right anterior cerebral artery
    'I63542',  # Cerebral infarction due to embolism of left anterior cerebral artery
    'I63543',  # Cerebral infarction due to embolism of bilateral anterior cerebral arteries
    'I63549',  # Cerebral infarction due to embolism of anterior cerebral artery, unspecified side
    'I6359',   # Cerebral infarction due to embolism of other specified cerebral arteries
    'I636',    # Cerebral infarction due to thrombosis or embolism of multiple cerebral arteries
    'I638',    # Other cerebral infarctions
    'I6381',
    'I6389',  # Other cerebral infarction
    'I639',   # Cerebral infarction, unspecified
    'I6350',  # Cerebral infarction due to unspecified occlusion or stenosis of middle cerebral artery
    'I63511', # Cerebral infarction due to occlusion or stenosis of middle cerebral artery with cerebral embolism
    'I63512', # Cerebral infarction due to occlusion or stenosis of middle cerebral artery with cerebral thrombosis
    'I63513', # Cerebral infarction due to occlusion or stenosis of middle cerebral artery with cerebral embolism, unspecified
    'I63519', # Cerebral infarction due to occlusion or stenosis of middle cerebral artery, unspecified
    'I63521', # Cerebral infarction due to occlusion or stenosis of anterior cerebral artery with cerebral embolism
    'I63522', # Cerebral infarction due to occlusion or stenosis of anterior cerebral artery with cerebral thrombosis
    'I63523', # Cerebral infarction due to occlusion or stenosis of anterior cerebral artery with cerebral embolism, unspecified
    'I63529', # Cerebral infarction due to occlusion or stenosis of anterior cerebral artery, unspecified
    'I63531', # Cerebral infarction due to occlusion or stenosis of posterior cerebral artery with cerebral embolism
    'I63532', # Cerebral infarction due to occlusion or stenosis of posterior cerebral artery with cerebral thrombosis
    'I63533', # Cerebral infarction due to occlusion or stenosis of posterior cerebral artery with cerebral embolism, unspecified
    'I63539', # Cerebral infarction due to occlusion or stenosis of posterior cerebral artery, unspecified
    'I63541', # Cerebral infarction due to occlusion or stenosis of cerebellar artery with cerebral embolism
    'I63542', # Cerebral infarction due to occlusion or stenosis of cerebellar artery with cerebral thrombosis
    'I63543', # Cerebral infarction due to occlusion or stenosis of cerebellar artery with cerebral embolism, unspecified
    'I63549', # Cerebral infarction due to occlusion or stenosis of cerebellar artery, unspecified
    'I6359',  # Cerebral infarction due to occlusion or stenosis of other specified artery, unspecified
    'I636',   # Cerebral infarction due to cerebral venous thrombosis, nonpyogenic
    'I638',   # Other cerebral infarction
    'I6381',  # Cerebral infarction due to other specified occlusion or stenosis of precerebral arteries
    'I6389',  # Other cerebral infarction
    'I639')   # Cerebral infarction, unspecified

# here, any hemorrhagic stroke
hem_stroke_icd_9 <-
  c('430',    # Subarachnoid hemorrhage
    '431',    # Intracerebral hemorrhage
    '4320',   # Nontraumatic extradural hemorrhage
    '4321',   # Subdural hemorrhage
    '4329')   # Unspecified intracranial hemorrhage

hem_stroke_icd_10 <-
  c("1609",   # Nontraumatic intracerebral hemorrhage, unspecified
    "1619",   # Nontraumatic subarachnoid hemorrhage, unspecified
    "1621",   # Nontraumatic subdural hemorrhage
    "16200",  # Nontraumatic extradural hemorrhage, unspecified
    "1629")   # Nontraumatic intracranial hemorrhage, unspecified



# Respiratory-related codes -----------------------------------------------

# at the moment, we're not distinguishing between respiratory conditions likley
# caused by power outage (heat, exertion, accidents, cold, or lack of food or
# water) and we're just putting them all in

resp_icd_9 <-
  c("460",    # Acute nasopharyngitis (common cold)
    "461",    # Acute sinusitis
    "462",    # Acute pharyngitis
    "463",    # Acute tonsillitis
    "464",    # Acute laryngitis and tracheitis
    "465",    # Acute upper respiratory infections of multiple or unspecified sites
    "466",    # Acute bronchitis
    "470",    # Deviated nasal septum
    "471",    # Nasal polyps
    "472",    # Chronic pharyngitis and nasopharyngitis
    "473",    # Chronic sinusitis
    "474",    # Chronic tonsillitis and adenoiditis
    "475",    # Peritonsillar abscess
    "476",    # Chronic laryngitis and laryngotracheitis
    "477",    # Allergic rhinitis
    "478",    # Other diseases of upper respiratory tract
    "480",    # Viral pneumonia
    "481",    # Pneumococcal pneumonia
    "482",    # Other bacterial pneumonia
    "483",    # Pneumonia due to other specified organisms
    "484",    # Pneumonia in infectious diseases classified elsewhere
    "485",    # Bronchopneumonia, organism unspecified
    "486",    # Pneumonia, organism unspecified
    "487",    # Influenza
    "488",    # Influenza due to certain identified viruses
    "490",    # Bronchitis, not specified as acute or chronic
    "491",    # Chronic bronchitis
    "492",    # Emphysema
    "493",    # Asthma
    "494",    # Bronchiectasis
    "495",    # Extrinsic allergic alveolitis
    "496",    # Chronic obstructive pulmonary disease, unspecified
    "500",    # Coal workers' pneumoconiosis
    "501",    # Asbestosis
    "502",    # Pneumoconiosis due to other silica or silicates
    "503",    # Pneumoconiosis due to other inorganic dust
    "504",    # Pneumonopathy due to inhalation of other dust
    "505",    # Pneumoconiosis, unspecified
    "506",    # Respiratory conditions due to chemical fumes and vapors
    "507",    # Pneumonitis due to solids and liquids
    "508",    # Respiratory conditions due to other external agents
    "510",    # Empyema
    "511",    # Pleural effusion, not elsewhere classified
    "512",    # Spontaneous pneumothorax
    "513",    # Abscess of lung and mediastinum
    "514",    # Pulmonary congestion and hypostasis
    "515",    # Postinflammatory pulmonary fibrosis
    "516",    # Other alveolar and parietoalveolar pneumonopathies
    "517",    # Lung involvement in conditions classified elsewhere
    "518",    # Other diseases of lung
    "519")    # Other diseases of respiratory system

resp_icd_10 <-
  c('J00',    # Acute nasopharyngitis (common cold)
    'J01',    # Acute sinusitis
    'J01.0',  # Acute maxillary sinusitis
    'J01.1',  # Acute frontal sinusitis
    'J01.2',  # Acute ethmoidal sinusitis
    'J01.3',  # Acute sphenoidal sinusitis
    'J01.4',  # Acute pansinusitis
    'J01.9',  # Acute sinusitis, unspecified
    'J02',    # Acute pharyngitis
    'J03',    # Acute tonsillitis
    'J04',    # Acute laryngitis and tracheitis
    'J05',    # Acute obstructive laryngitis [croup] and epiglottitis
    'J06',    # Acute upper respiratory infections of multiple or unspecified sites
    'J10',    # Influenza due to other identified influenza virus
    'J10.0',  # Influenza due to seasonal influenza virus with pneumonia
    'J10.1',  # Influenza due to seasonal influenza virus with other respiratory manifestations
    'J10.8',  # Influenza due to other seasonal influenza virus with other manifestations
    'J10.9',  # Influenza due to seasonal influenza virus, unspecified
    'J11',    # Influenza due to unidentified influenza virus
    'J11.0',  # Influenza due to unidentified influenza virus with pneumonia
    'J11.1',  # Influenza due to unidentified influenza virus with other respiratory manifestations
    'J11.8',  # Influenza due to unidentified influenza virus with other manifestations
    'J11.9',  # Influenza due to unidentified influenza virus, unspecified
    'J12',    # Viral pneumonia, not elsewhere classified
    'J13',    # Pneumonia due to Streptococcus pneumoniae
    'J14',    # Pneumonia due to Haemophilus influenzae
    'J15',    # Bacterial pneumonia, not elsewhere classified
    'J16',    # Pneumonia due to other infectious organisms
    'J17',    # Pneumonia in diseases classified elsewhere
    'J18',    # Pneumonia, unspecified
    'J20',    # Acute bronchitis
    'J20.0',  # Acute bronchitis due to Mycoplasma pneumoniae
    'J20.1',  # Acute bronchitis due to Chlamydophila pneumoniae
    'J20.2',  # Acute bronchitis due to Streptococcus pneumoniae
    'J20.3',  # Acute bronchitis due to Haemophilus influenzae
    'J20.4',  # Acute bronchitis due to Staphylococcus aureus
    'J20.5',  # Acute bronchitis due to other specified organisms
    'J20.9',  # Acute bronchitis, unspecified
    'J21',    # Acute bronchiolitis
    'J21.0',  # Acute bronchiolitis due to respiratory syncytial virus
    'J21.1',  # Acute bronchiolitis due to other viral agents
    'J21.8',  # Acute bronchiolitis due to other specified agents
    'J21.9',  # Acute bronchiolitis, unspecified
    'J22',    # Unspecified acute lower respiratory infection
    'J30',    # Allergic rhinitis
    'J30.0',  # Allergic rhinitis due to pollen
    'J30.1',  # Allergic rhinitis due to other seasonal allergens
    'J30.2',  # Perennial allergic rhinitis
    'J30.3',  # Vasomotor rhinitis
    'J30.4',  # Allergic rhinitis due to other specified allergens
    'J30.9',  # Allergic rhinitis, unspecified
    'J31',    # Chronic rhinitis, chronic sinusitis, and other chronic diseases of the upper respiratory tract
    'J32',    # Chronic sinusitis
    'J32.0',  # Chronic maxillary sinusitis
    'J32.1',  # Chronic frontal sinusitis
    'J32.2',  # Chronic ethmoidal sinusitis
    'J32.3',  # Chronic sphenoidal sinusitis
    'J32.4',  # Chronic pansinusitis
    'J32.9',  # Chronic sinusitis, unspecified
    'J33',    # Nasal polyp
    'J33.0',  # Polyp of maxillary sinus
    'J33.1',  # Polyp of frontal sinus
    'J33.2',  # Polyp of ethmoidal sinus
    'J33.3',  # Polyp of sphenoidal sinus
    'J33.9',  # Nasal polyp, unspecified
    'J34',    # Other disorders of nose and nasal sinuses
    'J34.0',  # Deviated nasal septum
    'J34.1',  # Atresia of nasal duct
    'J34.2',  # Obstructive hypertrophy of nasal turbinates
    'J34.8',  # Other specified disorders of nose and nasal sinuses
    'J34.9',  # Disorder of nose and nasal sinuses, unspecified
    'J35',    # Chronic diseases of tonsils and adenoids
    'J35.0',  # Hypertrophy of tonsils
    'J35.1',  # Hypertrophy of adenoids
    'J35.2',  # Hypertrophy of tonsils and adenoids
    'J35.9',  # Chronic disease of tonsils and adenoids, unspecified
    'J36',    # Peritonsillar abscess
    'J37',    # Chronic laryngitis and laryngotracheitis
    'J37.0',  # Chronic laryngitis
    'J37.1',  # Chronic laryngotracheitis
    'J37.9',  # Chronic laryngitis and laryngotracheitis, unspecified
    'J38',    # Disorders of vocal cords and larynx
    'J38.0',  # Paralysis of vocal cords
    'J38.1',  # Nodules of vocal cords
    'J38.2',  # Polyp of vocal cords
    'J38.3',  # Cyst of vocal cords
    'J38.4',  # Tumor of vocal cords
    'J38.5',  # Other disorders of vocal cords
    'J39',    # Other diseases of larynx and trachea
    'J39.0',  # Tracheostomy complications
    'J39.1',  # Stenosis of larynx and trachea
    'J39.8',  # Other specified diseases of larynx and trachea
    'J39.9',  # Disease of larynx and trachea, unspecified
    'J40',    # Bronchitis, not specified as acute or chronic
    'J41',    # Simple and mucopurulent chronic bronchitis
    'J41.0',  # Simple chronic bronchitis
    'J41.1',  # Mucopurulent chronic bronchitis
    'J41.8',  # Other chronic bronchitis
    'J41.9',  # Chronic bronchitis, unspecified
    'J42',    # Chronic bronchitis, unspecified
    'J43',    # Emphysema
    'J43.0',  # Centrilobular emphysema
    'J43.1',  # Panlobular emphysema
    'J43.9',  # Emphysema, unspecified
    'J44',    # Other chronic obstructive pulmonary disease
    'J44.0',  # Chronic obstructive pulmonary disease with acute lower respiratory infection
    'J44.1',  # Chronic obstructive pulmonary disease with acute exacerbation
    'J44.9',  # Chronic obstructive pulmonary disease, unspecified
    'J45',    # Asthma
    'J45.0',  # Predominantly allergic asthma
    'J45.1',  # Non-allergic asthma
    'J45.8',  # Mixed asthma
    'J45.9',  # Asthma, unspecified
    'J46',    # Status asthmaticus
    'J47',    # Bronchiectasis
    'J47.0',  # Localized bronchiectasis
    'J47.1',  # Diffuse bronchiectasis
    'J47.9',  # Bronchiectasis, unspecified
    'J60',    # Coal workers' pneumoconiosis
    'J61',    # Pneumoconiosis due to asbestos
    'J62',    # Pneumoconiosis due to other silica or silicates
    'J63',    # Pneumoconiosis due to other dust
    'J63.0',  # Pneumoconiosis due to dust containing silica
    'J63.1',  # Pneumoconiosis due to dust containing asbestos
    'J63.2',  # Pneumoconiosis due to dust containing coal
    'J63.3',  # Pneumoconiosis due to other dust
    'J63.8',  # Pneumoconiosis due to other specified dust
    'J63.9',  # Pneumoconiosis due to unspecified dust
    'J64',    # Pneumoconiosis, unspecified
    'J65',    # Chronic bronchitis due to pneumoconiosis
    'J66',    # Airway disease due to external agents
    'J67',    # Respiratory conditions due to fumes
    'J68',    # Respiratory conditions due to inhalation of other substances
    'J69',    # Pneumonitis due to solids and liquids
    'J70',    # Respiratory conditions due to radiation
    'J80',    # Acute respiratory distress syndrome
    'J81',    # Pulmonary edema
    'J82',    # Pulmonary eosinophilia
    'J83',    # Pulmonary alveolar proteinosis
    'J84',    # Interstitial lung disease
    'J85',    # Abscess of lung
    'J86',    # Empyema of lung
    'J87',    # Other diseases of pleura
    'J88',    # Other disorders of lung
    'J89',    # Respiratory conditions due to other specified causes
    'J90',    # Pleural effusion, not elsewhere classified
    'J91',    # Pleural effusion in conditions classified elsewhere
    'J92',    # Pneumothorax
    'J93',    # Other air in pleural cavity
    'J94',    # Other diseases of pleura
    'J95',    # Postoperative respiratory disorders
    'J96',    # Respiratory failure
    'J97',    # Respiratory complications of other diseases
    'J98',    # Other respiratory disorders
    'J99')    # Respiratory disorders in diseases classified elsewhere


all_icd_codes <- list(
  mi_icd_10 = mi_icd_10,
  mi_icd_9 = mi_icd_9,
  afib_icd_10 = afib_icd_10,
  afib_icd_9 = afib_icd_9,
  hem_stroke_icd_10 = hem_stroke_icd_10,
  hem_stroke_icd_9 = hem_stroke_icd_9,
  hyp_icd_10 = hyp_icd_10,
  hyp_icd_9 = hyp_icd_9,
  istroke_icd_10 = istroke_icd_10,
  istroke_icd_9 = istroke_icd_9,
  resp_icd_10 = resp_icd_10,
  resp_icd_9 = resp_icd_9,
  cvd_no_hem_no_hyp = c(
    mi_icd_10,
    mi_icd_9,
    afib_icd_10,
    afib_icd_9,
    istroke_icd_10,
    istroke_icd_9
  ),
  cvd_no_hyp = c(
    mi_icd_10,
    mi_icd_9,
    afib_icd_10,
    afib_icd_9,
    istroke_icd_10,
    istroke_icd_9,
    hem_stroke_icd_10,
    hem_stroke_icd_9
  ),
  all_cvd = c(
    mi_icd_10,
    mi_icd_9,
    afib_icd_10,
    afib_icd_9,
    istroke_icd_10,
    istroke_icd_9,
    hem_stroke_icd_10, 
    hem_stroke_icd_9,
    hyp_icd_10,
    hyp_icd_9),
  resp = c(resp_icd_10, resp_icd_9)
)

write_rds(all_icd_codes, here("data", "all_icd_codes.RDS"))
