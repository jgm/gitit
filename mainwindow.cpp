#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "gitstatusmodel.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    gitStatusModel(new GitStatusModel)
{
    ui->setupUi(this);
    ui->changedFileslistView->setModel(gitStatusModel);
}

MainWindow::~MainWindow()
{
    delete ui;
    delete gitStatusModel;
}
