from pydub import AudioSegment
import math
import numpy as np
import pandas as pd
rng = np.random.RandomState(0)

df = pd.read_excel('/Users/liesbethgijbels/Desktop/Online PA Project/reward_sounds/rewardsounds.xlsx', sheet_name='Sheet1')

for x in range(17):  
    sound1 = AudioSegment.from_file('/Users/liesbethgijbels/Desktop/Online PA Project/reward_sounds/'+df['audio'][x], format="wav")  # read audio file
    loudness =20 * math.log10(sound1.rms)
    print(loudness)
    sound1 = sound1 + (60-(20 * math.log10(sound1.rms)))
    loudness =20 * math.log10(sound1.rms)
    print(loudness)
    newfilename=df["name"][x]+"_60.wav" # determine filename
    print("new file name", newfilename)
    file_handle = sound1.export(newfilename, format="wav")  #save as wavfile


