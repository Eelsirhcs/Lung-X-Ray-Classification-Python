#%% EXPLORATION
# The main goal of this script is to explore the data inputs of the project.
## Content:

#%% Packages
# Packages to be used
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

#import keras
from keras.models import Sequential, load_model
#from keras.wrappers.scikit_learn import KerasClassifier
from keras.wrappers.scikit_learn import Sequential as Sequential_k
from keras.layers import Dense
from keras.layers import Dropout
from keras.layers import Flatten
from keras.layers.convolutional import Conv2D
from keras.layers.convolutional import MaxPooling2D#, AveragePooling2D
from keras.utils import np_utils, plot_model
from keras import backend as K
K.set_image_dim_ordering('th')

#from sklearn import linear_model, metrics, ensemble, svm, gaussian_process, tree, neural_network
from sklearn.model_selection import train_test_split#, cross_val_score, cross_val_predict, KFold
from sklearn import metrics
#from sklearn.model_selection import GridSearchCV
#from sklearn.feature_selection import SelectFromModel
#from sklearn.preprocessing import StandardScaler

#from random import sample
from PIL import Image
#import lodgepole.image_tools as lit
from os import listdir
from os.path import join#, isfile

import tensorflow as tf
config = tf.ConfigProto()
config.gpu_options.allow_growth = True
sess = tf.Session(config=config)

#%% Main directories settings
# Work directories and folders to be used.
mainDir = "C:\\Users\\wgsal\\OneDrive\\Documentos\\Mis cosas\\GSU\\2020-I\\Machine Learning\\MSA8150 Projects 2020\\BP-Lung Xray\\"
imFolders = ['NORMAL', 'Type A', 'Type B', 'Type C']
testFolder = ['testImages']


#%% FUNCTIONS: In this sections we defined every function we are going to use.
# -----------------------------------------------------------------------------
# LOADING DATA
# Every picture is stored in X (dict). Y has the category of each picture in X,
# and ids has the name of each picture (just in case).
def readData(dir_, folders):
    classes = folders
    Y = list()
    X = dict()
    ids = list()
    n = 0
    count = 0
    for f in folders:
        print('Loading images from folder - ' + f, end = ' ')
        folder = join(mainDir, f)
        imNames = listdir(folder)
        count1 = 0
        for i in imNames:
            imPath = join(folder, i)
            X[i] = Image.open(imPath).convert('L')
            Y.append(n)
            ids.append(i)
            count = count + 1
            count1 = count1 + 1
        n = n + 1
        print(' -', count1, 'images')
    Y = np_utils.to_categorical(Y)
    print('Images loaded: ', count)
    
    # Saving original X, Y, and ids
    np.save('originalX', X)
    np.save('originalY', Y)
    np.save('original_ids', ids)
    return X, Y, ids, classes

#X, Y, ids, classes = readData(dir_ = mainDir, folders = imFolders)

# -----------------------------------------------------------------------------
# FUNCTION TO SHOW SOME IMAGES
# Function to plot a small sample of images, maybe for the report.
def printSample(X, Y, ids, classes, toPlot = [0, 1, 2, 3], size = 4):
    # The graph plots the images selected in **ids**    
    # Grapgh settings
    base = size
    with plt.style.context('ggplot'):
        plt.figure(num = 1, figsize = [base * 4, base])
        
        n = 0
        for i in toPlot:
            n = n + 1
            plt.subplot(1, size, n)
            plt.imshow(X[ids[i]], cmap=plt.get_cmap('gray'))
            cat = classes[int(np.dot(Y[i], [0, 1, 2, 3]))]
            plt.title(cat, fontsize = 16)
    return

#printSample(X, Y, ids, classes, [0, 1212, 1212 + 643, 1212 + 643 + 253], 4)

# -----------------------------------------------------------------------------
# DATA PRE PROCESSING
# Function to convert the dictionary images into one big standaryzed matrix.
def dict2mat(X, Y, ids):
    sizex = 1024
    sizey = 1024
    sizeN = len(X)
    newX = np.zeros((sizeN, 1, sizex, sizey), dtype = 'float32')
    newY = np.zeros((sizeN, 4), dtype = 'float32')
    count = 0
    errors = list()
    for k in X.keys():
        try:
            newX[count, :, :, :] = np.asarray(X[k]) / 255
            newY[count, :] = Y[count, :]
            count = count + 1
        except:
            errors.append(k)
            ids.remove(k)
    newX = newX[0:(sizeN-len(errors)), :, :, :]
    newY = newY[0:(sizeN-len(errors)), :]
    print(len(errors), 'images ignored due to errors')
    
    # Saving
    np.save('processedX', newX)
    np.save('processedY', newY)
    np.save('processed_ids', ids)
    return newX, newY, ids, errors

#X, Y, ids, errors = dict2mat(X, Y, ids)

# -----------------------------------------------------------------------------
# DEFINITION OF MODEL'S STRUCTURE
# We decided to have a definite structure for our neural network. It is based
# in a mix of convolution, followed by a max pooling. The only thing that
# changes is the number of kernels and the pooling dimension size. To finalize,
# we added a flatten layer, 
def larger_model(nFilters, filterSize, poolingSize, num_classes = 4):
    layers = len(nFilters)
    # create model
    model = Sequential_k()
    for l in range(layers):
        if l == 0:
            model.add(Conv2D(nFilters[l],
                             (filterSize[l], filterSize[l]),
                             padding = 'same', #strides = (2, 2),
                             input_shape = (1, 1024, 1024),
                             activation = 'relu'))
        else:
            model.add(Conv2D(nFilters[l],
                             (filterSize[l], filterSize[l]),
                             padding = 'same',
                             activation = 'relu'))
        model.add(MaxPooling2D(pool_size=(poolingSize[l], poolingSize[l])))

    model.add(Flatten())
    model.add(Dense(256, activation='relu'))
    model.add(Dropout(0.5))
    model.add(Dense(num_classes, activation='softmax'))
    # Compile model
    model.compile(loss='categorical_crossentropy',
                  optimizer='adam',
                  metrics=['accuracy'])
    return model

# -----------------------------------------------------------------------------
# TRAIN-TEST SPLIT
def trainTestSplit(trainSize, testSize, randomState):
    # This function imports X and Y from disk, and split them into train and
    # test. If testSize is 0, the output isgoing to be only the hole dataset.
    X = np.load('processedX.npy')
    Y = np.load('processedY.npy')
    ids = np.load('processed_ids.npy')
    full = 0
    if testSize == 0:
        full = 1
        testSize = 1 - trainSize
        
    indTrain, indTest = train_test_split(range(len(ids)),
                                          train_size = trainSize,
                                          test_size = testSize,
                                          random_state = randomState)
    if full == 1:
        ind = np.concatenate([indTrain, indTest])
        X_train = X[ind, :, :, :]
        Y_train = Y[ind, :]
        X_test = 1
        Y_test = 1
    else:
        X_train = X[indTrain, :, :, :]
        Y_train = Y[indTrain, :]
        X_test = X[indTest, :, :, :]
        Y_test = Y[indTest, :]
    num_classes = Y_train.shape[1]
    return X_train, Y_train, X_test, Y_test, num_classes
    
#X_train, Y_train, X_test, Y_test, num_classes = trainTestSplit(0.8, 0.2, 9348956)


#%% LOADING DATA AN PREPROCESS
# Upload original data to workspace: Run only if the data is not stored as
# npy files
X, Y, ids, classes = readData(dir_ = mainDir, folders = imFolders)

# Data preprocess: Run only if preprocessed data is not storaged in npy files
X, Y, ids, errors = dict2mat(X, Y, ids)

# Train/test data split: This should run when all variables are recorded in disk
X_train, Y_train, X_test, Y_test, num_classes = trainTestSplit(0.8, 0, 99999999) #9348956)

#%% MODEL STRUCTURE, TRAINING, PREDICTION AND CLASSIFICATION REPORT
# Model creation
nFilters = [2, 8, 16, 32, 64]
filterSize = [3, 3, 3, 3, 3]
poolingSize = [2, 2, 2, 2, 2]
model = larger_model(nFilters, filterSize, poolingSize)
plot_model(model, to_file = 'Whole DS Model.png')

# Model fitting
model.fit(X_train, Y_train,
          validation_data = (X_test, Y_test),
          epochs = 12, batch_size = 256)

# Model prediction
y_true = np.dot(Y_test, [0,1,2,3])
y_pred = model.predict_classes(X_test)
report = metrics.classification_report(y_true, y_pred)
print(report)
print(metrics.confusion_matrix(y_true, y_pred))
print(model.summary())

# Saving model
model.save('Model')
#model = load_model("Model")

# Plot loss and accuracy during training
plt.style.use('ggplot')
base = 3
fig = plt.figure(num = 1, figsize = [2 * base * (1 + np.sqrt(5))/2, base])
plt.style.use('ggplot')
c1 = '0.5'
c2 = 'C0'
plt.subplot(121)
plt.title('Loss')
plt.plot(model.history.history['loss'], label='train', color = c1, linestyle = '--')
plt.plot(model.history.history['val_loss'], label='test', color = c2)
plt.legend(shadow = True, facecolor = '1')
# plot accuracy during training
plt.subplot(122)
plt.title('Accuracy')
plt.plot(model.history.history['acc'], label='train', color = c1, linestyle = '--')
plt.plot(model.history.history['val_acc'], label='test', color = c2)
plt.legend(shadow = True, facecolor = '1')
plt.show()

#%% MODEL FITTING WITH 100% OF THE DATA
# Model fitting - With all the data
model.fit(X_train, Y_train,
#          validation_data = (X_test, Y_test),
          epochs = 12, batch_size = 256)

# Model prediction
y_true = np.dot(Y_train, [0,1,2,3])
y_pred = model.predict_classes(X_train)
report = metrics.classification_report(y_true, y_pred)
print(report)
print(metrics.confusion_matrix(y_true, y_pred))
print(model.summary())

# Saving model
model.save('finalModel')
#model = load_model("finalModel")

#%% UPLOADING, PREPROCESS AND PREDICTIONS ON TEST DATA

X, Y, ids, classes = readData(dir_ = mainDir, folders = ['Test Images'])
X, Y, ids, errors = dict2mat(X, Y, ids)
y_test_pred = model.predict_classes(X)