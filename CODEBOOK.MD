Getting and Cleaning Data Course Project - Code Book
1. The file features.txt has all the column names in the raw data set.
2. Sample output of the tidy dataset is given below:
> head(vfinalData)
  activityId subjectId timeBodyAccMagnitudeMean timeBodyAccMagnitudeStdDev
1          1         7              -0.09188078                 -0.2193562
2          1         5              -0.06040238                 -0.3184467
3          1         6              -0.17864884                 -0.3309294
4          1        23              -0.16544768                 -0.2496463
5          1         7              -0.05080089                 -0.1929635
6          1         7              -0.20943372                 -0.3439433
  timeGravityAccMagnitudeMean timeGravityAccMagnitudeStdDev timeBodyAccJerkMagnitudeMean
1                 -0.09188078                    -0.2193562                   -0.2212705
2                 -0.06040238                    -0.3184467                   -0.1772447
3                 -0.17864884                    -0.3309294                   -0.1542025
4                 -0.16544768                    -0.2496463                   -0.1649508
5                 -0.05080089                    -0.1929635                   -0.1670148
6                 -0.20943372                    -0.3439433                   -0.3906790
  timeBodyAccJerkMagnitudeStdDev timeBodyGyroMagnitudeMean timeBodyGyroMagnitudeStdDev
1                    -0.08790417               -0.23188636                 -0.35877676
2                    -0.18946999               -0.29106290                 -0.45700393
3                    -0.11458085               -0.23214497                 -0.16900367
4                    -0.07143652               -0.09337262                  0.09497147
5                    -0.06740565               -0.31105774                 -0.26027357
6                    -0.26383091               -0.17464051                 -0.34537812
  timeBodyGyroJerkMagnitudeMean timeBodyGyroJerkMagnitudeStdDev freqBodyAccMagnitudeMean
1                   -0.35258183                      -0.2598570               -0.2241610
2                   -0.34617000                      -0.4318484               -0.1955860
3                   -0.18464348                      -0.1752300               -0.2129347
4                   -0.08623081                       0.1354233               -0.1348517
5                   -0.34247773                      -0.2548952               -0.1676230
6                   -0.47486330                      -0.2336683               -0.3803205
  freqBodyAccMagnitudeStdDev freqBodyAccJerkMagnitudeMean freqBodyAccJerkMagnitudeStdDev
1                 -0.3377600                  -0.10808845                    -0.06845567
2                 -0.5051218                  -0.16253574                    -0.23087424
3                 -0.5120523                  -0.09577360                    -0.13896673
4                 -0.4398540                  -0.07529004                    -0.07278292
5                 -0.3328146                  -0.08101258                    -0.05616894
6                 -0.4259079                  -0.29646073                    -0.22774662
  freqBodyGyroMagnitudeMean freqBodyGyroMagnitudeStdDev freqBodyGyroJerkMagnitudeMean
1                -0.3555950                  -0.4737873                   -0.29350547
2                -0.4141969                  -0.5892402                   -0.50854872
3                -0.1490698                  -0.3312052                   -0.23541469
4                 0.2018203                  -0.1916902                    0.06148761
5                -0.2549877                  -0.3944225                   -0.29446303
6                -0.3522259                  -0.4548342                   -0.27006915
  freqBodyGyroJerkMagnitudeStdDev activityType
1                      -0.2687860      WALKING
2                      -0.3819675      WALKING
3                      -0.1579364      WALKING
4                       0.1491723      WALKING
5                      -0.2558076      WALKING
6                      -0.2406118      WALKING