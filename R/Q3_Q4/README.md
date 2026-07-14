## data description - practical\_seminar

This data was collected at the practical seminars on 2-3 March and 8 May 2022 (Cohort 1)/ 22-23 April and 8 May 2023 (Cohort 2)

## variable description - practical\_seminar

* team: individual team ID
* FCI: FCI group according to Federation Cynologique Internationale(2025)
* federal state: the federal state of origin of each team, anonymised
* target: target species the dog was trained on: *F. spp.* complex, *A. artemisiifolia* or *I. glandulifera*
* date: date of the practical seminar
* year: year of the practical seminar, representing the cohorts
* preparation: Persons who prepared the test sites by placing the samples in the grid, anonymised
* leash: indicating whether the dog was on a leash or not during the search
* temp: temperature during the test in °C
* humidity: humidity during the test in %
* weather: weather conditions during the test: sunny, partly cloudy, cloudy, rainy
* wind: Wind strength during the test checked by observing trees following Isyumov N \& Davenport AG (1975); none, slight, intermittent, moderate, or severe, referring to Beaufort scale 0-1, 2, 3, 4, 5 or more, respectively
* wind direction: direction where the wind came from
* sample number: numer of samples hidden during one test
* sample condition: condition of the hidden sample in the test (fresh or frozen, referred to as "thawed")
* search: test session number (first, second or third test)
* sample: plant samples were numbered to ensure the dog is able to find different samples
* grid: grid where the specific plant sample was hidden
* size: length of the hidden sample in cm
* found\_dog: indicating whether the dog found the sample
* found\_handler: indicating whether the handler found the sample
* time: search time in seconds
* time\_orig: originally recorded time in minutes
* alert: indicating whether and how confident the dog alerted at the sample
* end: end time of the search
* false\_pos: number of false positive alerts
* search\_finished: indicating whether the team finished the search

## data description - survey

This data was collected using a questionnaire on training habits during the project.

## variable description - survey

* team: individual team ID
* age\_dog: age of the dog at the beginning of the training
* FCI: FCI group according to Federation Cynologique Internationale(2025)
* participation\_trainingwebinar\_live: Live participation in the online training seminars (yes, partly, no)
* training\_days\_week: average number of training days per week a team could afford
* session\_duration\_min: average length of a training session in minutes
* steps\_skipped: number of skipped steps in the detailed training plan (none, few, many)
* single\_blind: number of single blind searches (none, few, many)
* search\_area: average size of the search area of a training session in m²
* dried: were dried plants included in training? (1 = yes, 0 = no)
* fresh\_training: were fresh plants included in training? (1 = yes, 0 = no)
* nr\_individuals: Number of plant individuals used for training (none, few, many)
* plant\_parts: which/how many parts of the plant were used for training (1 = leaves, 2 = leaves and stem, 3 = leaves, stem and roots, 4 = leaves, stem, roots and flower)
* reward: type of reward used for the dog (food, toy, both)
* rew\_variety: did the reward vary, e.g. different types of food/toys (1 = yes, 0 = no)
* experience: team experience, 0 = none, 1 = dog sports, 2 = tracking and general scent work, 3 = scent matching or object search

## data description: teams\_points

This data includes variables which were used to calculate the team score.

## variable description: teams\_points

* team: individual team ID
* points: team score, teams received points for starting and finishing test sessions, points were multiplied by the proportion detected in practical seminar, numerical between 0 and 9
* fresh\_test: proportion of fresh samples hidden during the test
* diff\_goal: Mean of deviation from average duration to reach a training goal (goal achieved = yes) in phase 1 and 2 (online training), in days
* rel\_goal: Relative goal achievement, a normalized metric that quantifies the proportion of training steps in which a specific training goal was successfully achieved, relative to the total number of training steps filmed per team during phase 1 and 2 of online training; 0 indicates the goal was never achieved across any filmed step, 1 indicates the goal was achieved in every filmed step, values between 0 and 1 reflect partial achievement relative to the total available training steps filmed
* nr\_steps: number of training steps

## References

* Federation Cynologique Internationale. FCI Breeds Nomenclature. 2025. Available: https://www.fci.be/en/Nomenclature/
* Isyumov N, Davenport AG. The ground level wind environment in built-up areas. Proceedings of the 4th int Conf Wind effects on buildings and structures. London; 1975. pp. 420-422.

