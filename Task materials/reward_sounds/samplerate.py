# Imports
import time

import numpy as np
from matplotlib.image import imread
import pandas as pd

from expyfun import ExperimentController, get_keyboard_input
from expyfun.visual import Rectangle, RawImage
from expyfun.stimuli import vocode, play_sound, window_edges, read_wav, rms



#df = pd.read_excel('/Users/liesbethgijbels/Desktop/Online PA project/reward_sounds/rewardsounds.xlsx', sheet_name='Sheet1')
data, fs = read_wav('/Users/liesbethgijbels/Desktop/Online PA project/reward_sounds/reward1.wav')


""" for x in range(17):
 # audio file (files have fs=16000 -- need to interpolate until fs=48000 - used numpy for this)
    data, fs = read_wav('/Users/liesbethgijbels/Desktop/Online PA project/reward_sounds/'+df['audio'][x])
    print(fs)
    data = data * 0.01 / np.sqrt(np.mean(data * data))
    fs_new = 48000
    fs_ratio = fs/fs_new
    data_new = np.interp(
        np.arange(0, len(data[0]), fs_ratio), np.arange(0, len(data[0])), data[0])
    data_new = np.array([data_new])

    sound1 = AudioSegment.from_file(data_new, format="wav")  # read audio file
    loudness =20 * math.log10(sound1.rms)
    print(loudness)
    sound1 = sound1 + (60-(20 * math.log10(sound1.rms)))
    loudness =20 * math.log10(sound1.rms)
    print(loudness)
    newfilename=df["name"][x]+"60.wav" # determine filename
    print("new file name", newfilename)
    file_handle = sound1.export(newfilename, format="wav")  #save as wavfile """