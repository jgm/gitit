#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QStringListModel>
#include "existingProjectWizard.h"
#include "newProjectWizard.h"

class Configure;
class GitChangedStatusModel;
class GitCommand;
class GitStagedStatusModel;

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private:
    Ui::MainWindow *ui;
    Configure* configure;
    GitChangedStatusModel* gitChangedStatusModel;
    GitStagedStatusModel* gitStagedStatusModel;
    QStringListModel* gitIgnoredFilesModel;
    QString repo;
    ExistingProjectWizard* existingProjectWizard;
    NewProjectWizard* newProjectWizard;

    GitCommand* gitCommand;

signals:
    void repositoryChanged(QString repo);

private slots:
    void on_changeCurrentBranch_clicked();
    void on_deleteBranch_clicked();
    void on_mergeBranch_clicked();
    void on_renameBranch_clicked();
    void on_createNewBranch_clicked();
    void on_syncToButton_clicked();
    void on_reload_clicked();
    void on_shipButton_clicked();
    void on_gitAddButton_clicked();
    void on_actionOpen_triggered();
    void on_actionConfigure_triggered();
    void boxClicked();
    void about();
    void exit();
    void menuNew();
    void userManual();
    void activateExistingProjectWizard();
    void activateNewProjectWizard();
    void updateIgnoredModel(QStringList);
    void updateLog(QString);
public slots:
    void reload();
};

#endif // MAINWINDOW_H
