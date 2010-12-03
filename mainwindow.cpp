#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "gitchangedstatusmodel.h"
#include "configure.h"
#include <QFileDialog>
#include <QDebug>
#include <QMessageBox>
#include "gitcommand.h"
#include "gitstagedstatusmodel.h"
#include <QStringList>
#include <QInputDialog>
#include <QDesktopServices>
#include <QUrl>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    gitCommand(new GitCommand),
    configure(new Configure(this,gitCommand)),
    gitChangedStatusModel(new GitChangedStatusModel),
    gitStagedStatusModel(new GitStagedStatusModel),
    gitIgnoredFilesModel(new QStringListModel),
    existingProjectWizard( new ExistingProjectWizard),
    newProjectWizard( new NewProjectWizard)
{
    ui->setupUi(this);
    ui->changedFileslistView->setModel(gitChangedStatusModel);
    ui->stagedFilesListView->setModel(gitStagedStatusModel);
    ui->ignoredFilesListView->setModel(gitIgnoredFilesModel);
    //connecting slots and signals
    connect( gitCommand, SIGNAL(status(QStringList)), gitChangedStatusModel, SLOT(update(QStringList)));
    connect( gitCommand, SIGNAL(status(QStringList)), gitStagedStatusModel,  SLOT(update(QStringList)));

    connect( ui->actionAbout, SIGNAL( triggered() ), this, SLOT(about()) );
    connect( ui->actionExit, SIGNAL( triggered() ), this, SLOT(exit()) );
    connect( ui->actionNew, SIGNAL( triggered() ), this, SLOT(menuNew()) );
    connect( ui->actionUser_s_Manual, SIGNAL( triggered() ), this, SLOT(userManual()) );
    connect( ui->actionExisting_Project, SIGNAL(triggered()), this, SLOT(activateExistingProjectWizard()) );
    connect( ui->actionNew, SIGNAL(triggered()), this, SLOT(activateNewProjectWizard()) );
    connect( ui->actionNew_Project, SIGNAL(triggered()), this, SLOT(activateNewProjectWizard()));
    //using a builtin model here:
    connect( gitCommand, SIGNAL(lsIgnored(QStringList)), this, SLOT(updateIgnoredModel(QStringList)));
    connect( gitCommand, SIGNAL(log(QString))          , this, SLOT(updateLog(QString) ));
    connect( existingProjectWizard, SIGNAL(accepted()), this, SLOT(existingProjectWizardAccepted()));

}

MainWindow::~MainWindow()
{
    delete ui;
    delete gitChangedStatusModel;
    delete configure;
    delete gitStagedStatusModel;
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
    if(fileNames.count()>0)
    {
        repo = fileNames[0];
        this->gitCommand->setRepo(repo);
    }
    reload();
}

void MainWindow::boxClicked()
{
    ui->statusBar->showMessage("box", 15000);
}

void MainWindow::about()
{
    QMessageBox::about(this, tr("About Gitit"),
                       tr("Gitit, Created for CS440 Fall 2010<br>"
                          "For access to source code, goto http://github.com/bdenne2/gitit<br>"
                          "Bryanna Denney &lt;bryanna.denney@gmail.com&gt;<br>"
                          "Mike Salata &lt;mbaker22@uic.edu&gt;<br>"
                          "Steven Sennebogen &lt;ssennebo@cs.uic.edu&gt;"));
}

void MainWindow::exit()
{
    this->close();
}

void MainWindow::menuNew()
{

}

void MainWindow::userManual()
{

}

void MainWindow::activateExistingProjectWizard()
{
    existingProjectWizard->restart();
    existingProjectWizard->clear();
    existingProjectWizard->show();
}
void MainWindow::existingProjectWizardAccepted()
{
    QString path = existingProjectWizard->getGitPath();
    gitCommand->setRepo(path);
    reload();
}

void MainWindow::activateNewProjectWizard()
{
    newProjectWizard->restart();
    newProjectWizard->clear();
    newProjectWizard->show();
    gitCommand->setRepo(newProjectWizard->getGitPath());
    reload();
}

void MainWindow::updateIgnoredModel(QStringList files)
{
    gitIgnoredFilesModel->setStringList(files);
}

void MainWindow::updateLog(QString log)
{
    ui->gitLogTextEdit->setPlainText(log);
}

void MainWindow::on_gitAddButton_clicked()
{
    QItemSelectionModel* selectionModel = ui->changedFileslistView->selectionModel();
    QItemSelection itemSelection = selectionModel->selection();
    QModelIndexList indexList = itemSelection.indexes();
    for(int i=0; i < indexList.count(); ++i)
    {
        QString filename = QString(indexList.at(i).data().toString());
        gitCommand->add( filename );

        ui->statusBar->showMessage(filename,5000);
    }
    reload();
}

void MainWindow::reload()
{
    gitCommand->status();
    gitCommand->lsIgnored();
    gitCommand->log();
    ui->statusBar->showMessage(repo,30000);
}

void MainWindow::on_shipButton_clicked()
{
    QStringList args;
    QString description;
    description = ui->commitName->text();
    description += "\n\n" + ui->commitDescription->toPlainText();
    args << "commit" << "-m" << description;
    gitCommand->run(args);
    ui->commitDescription->setText("");
    ui->commitName->setText("");
    reload();
}

void MainWindow::on_reload_clicked()
{
    reload();
}

void MainWindow::on_createNewBranch_clicked()
{
    bool ok;
    QStringList args;
    QString newBranch = QInputDialog::getText(this,"Create Branch","Enter then name of the new Branch",QLineEdit::Normal,"",&ok);
    if(ok)
    {
        args << "checkout" << "-b" << "newBranch";
        gitCommand->run(args);
    }
}

void MainWindow::on_renameBranch_clicked()
{
    bool ok;
    QStringList args;
    QString newName = QInputDialog::getText(this,"Rename Branch","Enter then new name of the current Branch",QLineEdit::Normal,"",&ok);
    if(ok)
    {
        args << "branch" << "-M" << "newName";
        gitCommand->run(args);
    }
}

void MainWindow::on_mergeBranch_clicked()
{
    bool ok;
    QStringList args;
    QStringList branches = gitCommand->branchList();
    QString branch = QInputDialog::getItem(this,
                          "Merge Branch",
                          "Select Branch to merge into current",
                          branches,
                          0,
                          false,
                          &ok);
    QRegExp awesome("^..");
    awesome.setPatternSyntax(QRegExp::RegExp2);
    branch.replace(awesome,"");
    if(ok)
    {
        args << "merge" << branch;
        gitCommand->run(args);
    }
}

void MainWindow::on_deleteBranch_clicked()
{
    bool ok;
    QStringList args;
    QStringList branches = gitCommand->branchList();
    QString branch = QInputDialog::getItem(this,
                          "Delete Branch",
                          "Select Branch to Delete",
                          branches,
                          0,
                          false,
                          &ok);
    QRegExp awesome("^..");
    awesome.setPatternSyntax(QRegExp::RegExp2);
    branch.replace(awesome,"");
    if(ok)
    {
        args << "branch" << "-D" << branch;
        gitCommand->run(args);
    }
}

void MainWindow::on_changeCurrentBranch_clicked()
{
    bool ok;
    QStringList args;
    QStringList branches = gitCommand->branchList();
    QString branch = QInputDialog::getItem(this,
                          "Change Branch",
                          "Select Branch to Change to",
                          branches,
                          0,
                          false,
                          &ok);
    QRegExp awesome("^..");
    awesome.setPatternSyntax(QRegExp::RegExp2);
    branch.replace(awesome,"");
    if(ok)
    {
        args << "checkout" << branch;
        gitCommand->run(args);
    }
}

void MainWindow::on_syncToButton_clicked()
{
    bool ok;
    QStringList args;
    ///TODO: This needs to change to show a list of remote repos, not remote branches.
    QStringList branches = gitCommand->run(QStringList() << "remote" << "show");
    QString branch = QInputDialog::getItem(this,
                          "Push to remote repo",
                          "Select the remote repo to push too",
                          branches,
                          0,
                          false,
                          &ok);
    QRegExp awesome("^..");
    awesome.setPatternSyntax(QRegExp::RegExp2);
    branch.replace(awesome,"");
    if(ok)
    {
        args << "push" << branch;
        gitCommand->run(args);
    }
}

void MainWindow::on_syncFromButton_clicked()
{
    bool ok;
    QStringList args;
    ///TODO: This needs to change to show a list of remote repos, not remote branches.
    QStringList branches = gitCommand->remoteBranchList();
    QString branch = QInputDialog::getItem(this,
                          "Push to remote branch",
                          "Select the remote branch to push too",
                          branches,
                          0,
                          false,
                          &ok);
    //TODO: filter tracjking branch output better
    QRegExp awesome("^..");
    awesome.setPatternSyntax(QRegExp::RegExp2);
    branch.replace(awesome,"");

    QRegExp cleanup(" -> .*");
    cleanup.setPatternSyntax(QRegExp::RegExp2);
    branch.replace(cleanup,"");
    ui->statusBar->showMessage(branch, 3000);
    if(ok)
    {
        args << "merge" << branch;
        gitCommand->run(args);
    }
}

void MainWindow::on_gitIgnoreButton_clicked()
{
    QFile gitignore(repo + "/.gitignore");
    gitignore.open(QIODevice::Append);
    QTextStream out(&gitignore);


    QItemSelectionModel* selectionModel = ui->changedFileslistView->selectionModel();
    QItemSelection itemSelection = selectionModel->selection();
    QModelIndexList indexList = itemSelection.indexes();
    for(int i=0; i < indexList.count(); ++i)
    {
        QString filename = QString(indexList.at(i).data().toString());
        out << filename <<"\n";

        ui->statusBar->showMessage(filename,5000);
    }
    reload();

}

void MainWindow::on_actionUser_s_Manual_triggered()
{
    QDesktopServices::openUrl(QUrl("https://github.com/bdenne2/gitit/raw/master/userManual.pdf"));
}

void MainWindow::on_pushButton_clicked()
{
    gitCommand->run(QStringList() << "remote" << "update");
    ui->statusBar->showMessage("updated",1500);
}
