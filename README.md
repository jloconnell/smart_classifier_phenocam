# PhenoCam Smart Classifer

The Smart Classifier Algorithm for the "GCE Sapelo" PhenoCam

See: O’Connell, J. L., and M. Alber. 2016. A smart classifier for extracting environmental data from digital image time-series: Applications for PhenoCam data in a tidal salt marsh. Environmental Modelling & Software 84:134–139. http://dx.doi.org/10.1016/j.envsoft.2016.06.025

PhenoCams are part of a national network of automated digital cameras used to assess vegetation
phenology transitions. Effectively analyzing PhenoCam time-series involves eliminating scenes with poor
solar illumination or high cover of non-target objects such as water. We created a smart classifier to
process images from the “GCESapelo” PhenoCam, which photographs a regularly-flooded salt marsh. The
smart classifier, written in R, assigns pixels to target (vegetation) and non-target (water, shadows, fog
and clouds) classes, allowing automated identification of optimal scenes for evaluating phenology. When
compared to hand-classified validation images, the smart classifier identified scenes with optimal
vegetation cover with 96% accuracy and other object classes with accuracies ranging from 86 to 100%.
Accuracy for estimating object percent cover ranged from 74 to 100%. Pixel-classification with the smart
classifier outperformed previous approaches (i.e. indices based on average color content within ROIs) and
reduced variance in phenology index time-series. It can be readily adapted for other applications.

You can find a reproducible example to download and run these scripts at the lab website: https://www.landscapemodeling.net/research.html
