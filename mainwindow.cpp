#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "gitstatusmodel.h"
#include "configure.h"
#include <QFileDialog>
#include <QDebug>
MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    configure(new Configure(this)),
    gitStatusModel(new GitStatusModel)
{
    ui->setupUi(this);
    ui->changedFileslistView->setModel(gitStatusModel);
    //connect(this,SIGNAL(repositoryChanged(git_repository*)),gitStatusModel,SLOT(update(git_repository*)));
    //connecting slots and signals
    connect(ui->box, SIGNAL(linkActivated(QString)), this, SLOT(boxClicked()) );
}

MainWindow::~MainWindow()
{
    delete ui;
    delete gitStatusModel;
    delete configure;

}

void MainWindow::on_actionConfigure_triggered()
{
    configure->show();
}

void MainWindow::on_actionOpen_triggered()
{
    QStringList fileNames;

    QFileDialog dialog(this);
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setViewMode(QFileDialog::Detail);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dialog.exec())
        fileNames = dialog.selectedFiles();

    repo = fileNames[0];
}

void MainWindow::boxClicked()
{
    ui->statusBar->showMessage("box", 15);
}
