#ifndef EXISTINGPROJECTWIZARD_H
#define EXISTINGPROJECTWIZARD_H

#include<QWizard>
#include<QWizardPage>
#include<QLabel>
#include<QVBoxLayout>
#include<QButtonGroup>
#include<QRadioButton>
#include<QLineEdit>

class GitCommand;
class ExistingProjectWizard: public QWizard
{
    Q_OBJECT

public:
   ExistingProjectWizard();
   ~ExistingProjectWizard();
   QString getGitPath();
private:
    GitCommand* gitCommand;
    void createIntroPage();
    void createGetLocalPath();
    void createGetRemotePath();
    void createGetLocalDirectory();
    void createEndNew();
    QWizardPage* introPage;
    QWizardPage* localPath;
    QWizardPage* remotePath;
    QWizardPage* localDirectory;
    QWizardPage* conclusion;
    QStringList path;
    int nextId() const;

    //the following are fields that I cannot figure out how to access from other methods
    QLineEdit* pathDisplay;
    QLabel* hiddenComment;
    QLineEdit* userRemotePath;
    QLineEdit* userLocalDirectory;
    QRadioButton* localButton;
    QRadioButton* remoteButton;


signals:
    void setPath();

private slots:
    void getPath();
    void displayHiddenComment(int index);
    void getPathToLocalDirectory();
    void createRepo();
};

#endif // EXISTINGPROJECTWIZARD_H
