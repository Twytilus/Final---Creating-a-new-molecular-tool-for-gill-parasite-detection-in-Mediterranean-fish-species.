# Homework - creating primers for a qPCR reaction.
In order to amplify a specific DNA fragment of an organism of interest, we need to create an appropriate set of primers. 

First, however, we need to find a stable Reference Gene, as well as a Response Gene of interest. Both require a set of primers for qPCR. 
## Step 1 - Ref. Gene.
*Vibrio alginolyticus* is a Gram-negative marine bacterium. It is medically important since it causes otitis and wound infection. It is also present in the bodies of animals such as pufferfish, where it is responsible for the production of the potent neurotoxin, tetrodotoxin, as well living on the gills of many fish species as a pathogen. It is expected that during my work, I will encounter this bacteria often.

GyrB gene is considered to be a stable housekeeping gene for the species, helpful for the purposes of its identification. [(Zhou S, Hou Z, Li N, Qin Q (2007) Development of a SYBR Green I real-time PCR for quantitative detection of Vibrio alginolyticus in seawater and seafood. J Appl Microbiol 103:1897–1906)](https://academic.oup.com/jambio/article-abstract/103/5/1897/6719271?redirectedFrom=fulltext&login=false) 

It is appropriate for our task, so I will create a primer for the gene [(found on the NCBI)](https://www.ncbi.nlm.nih.gov/nuccore/PP072268.1?report=fasta) using the primer3 tool. By setting the bp size of the product to 150-170 I get the following primers.

![primersref](../images/Ref.gene%20primers.JPG)

## Step 2 Response Gene.
The groEL gene is a housekeeping gene as well, and plays a vital role in the control of cellular stress in bacterial cells. When bacterial cells are exposed to environmental stress or enter into host tissue, the groEL gene is induced to express at a markedly higher level to protect bacterial cells from damage. [(Ahmed, R., Rafiquzaman, S. M., Hossain, M. T., Lee, J. M., & Kong, I. S. (2016). Species-specific detection of Vibrio alginolyticus in shellfish and shrimp by real-time PCR using the groEL gene. Aquaculture international, 24, 157-170.)](https://link.springer.com/article/10.1007/s10499-015-9916-5)

It is appropriate for our task, so I will create a primer for the gene [(found on the NCBI)](https://www.ncbi.nlm.nih.gov/nuccore/KX094897.1?report=fasta) using the primer3 tool. By setting the bp size of the product to 150-170 I get the following primers. 

![primerresp](../images/response%20gene%20primers.JPG)

After inducing stress to those genes, for example, an increase of temperature, as it is the easiest environmental stress to emulate *in vitro*. After measuring the expression level of the groEL gene, I expect it to be higher when compared to the control control group, 