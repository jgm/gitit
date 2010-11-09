#-------------------------------------------------
#
# Project created by QtCreator 2010-10-28T20:47:00
#
#-------------------------------------------------

QT       += core gui

TARGET = gitit
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    history.cpp \
    commit.cpp \
    currentuserinfo.cpp \
    gitstatusmodel.cpp

HEADERS  += mainwindow.h \
    history.h \
    commit.h \
    currentuserinfo.h \
    gitstatusmodel.h

FORMS    += mainwindow.ui
