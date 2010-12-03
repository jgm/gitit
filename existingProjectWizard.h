#ifndef EXISTINGPROJECTWIZARD_H
#define EXISTINGPROJECTWIZARD_H

#include<QWizard>
#include<QWizardPage>
#include<QLabel>
#include<QVBoxLayout>
#include<QButtonGroup>
#include<QRadioButton>
#include<QLineEdit>
#include<QComboBox>

class ExistingProjectWizard: public QWizard
{
    Q_OBJECT

public:
   ExistingProjectWizard();
   ~ExistingProjectWizard();
   void clear();
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
    int nextId() const;

    //the following are fields that I cannot figure out how to access from other methods
    QLineEdit* pathDisplay;
    QLabel* hiddenComment;
    QLineEdit* userRemotePath;
    QLineEdit* userLocalDirectory;
    QRadioButton* localButton;
    QRadioButton* remoteButton;
    QComboBox* remoteOption;

signals:
    void setPath();

private slots:
    void getPath();
    void displayHiddenComment(int index);
    void getPathToLocalDirectory();
};

#endif // EXISTINGPROJECTWIZARD_H
