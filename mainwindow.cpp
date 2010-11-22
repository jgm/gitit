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
    gitStatusModel(new GitStatusModel),
    repo(NULL)
{
    ui->setupUi(this);
    ui->changedFileslistView->setModel(gitStatusModel);
    connect(this,SIGNAL(repositoryChanged(git_repository*)),gitStatusModel,SLOT(update(git_repository*)));
    git_repository_open(&repo,"/Users/hef/projects/cs440/gitit/.git/");
}

MainWindow::~MainWindow()
{
    delete ui;
    delete gitStatusModel;
    delete configure;
    if(repo!=NULL)
    {
        git_repository_free(repo);
    }
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
    qDebug() << "Path:" << fileNames[0];

    int return_value = git_repository_open(&repo,(fileNames[0] + "/.git/").toLatin1());
    qDebug() << "Repo open return Value" << return_value;

    if(repo!=NULL)
    {
        qDebug() << "about to emit repositoryChanged(" << repo << ")";
        emit repositoryChanged(repo);
        //ui->changedFileslistView->setModel(gitStatusModel);
        qDebug() << "signal emitted";
    }
    else
    {
        qDebug() << "New Repo not opened.";
    }
}
