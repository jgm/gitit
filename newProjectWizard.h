#ifndef NEWPROJECTWIZARD_H
#define NEWPROJECTWIZARD_H

#include<QWizard>
#include<QWizardPage>
#include<QLabel>
#include<QVBoxLayout>
#include<QButtonGroup>
#include<QRadioButton>
#include<QLineEdit>

class NewProjectWizard: public QWizard
{
    Q_OBJECT

public:
   NewProjectWizard();
   ~NewProjectWizard();
private:
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
    //the following are fields that I cannot figure out how to access from other methods
    QLineEdit* pathDisplay;
    QLabel* hiddenComment;
    QLineEdit* userRemotePath;
    QLineEdit* userLocalDirectory;

signals:
    void setPath();

private slots:
    void getPath();
    void displayHiddenComment(int index);
    void getPathToLocalDirectory();
};

#endif // NEWPROJECTWIZARD_H
