#ifndef NEWPROJECTWIZARD_H
#define NEWPROJECTWIZARD_H

#include<QWizard>
#include<QWizardPage>
#include<QLabel>
#include<QVBoxLayout>
#include<QButtonGroup>
#include<QRadioButton>
#include<QLineEdit>
#include<QFileDialog>
#include<QPushButton>

class NewProjectWizard : public QWizard
{
    Q_OBJECT

public:
    NewProjectWizard();
    ~NewProjectWizard();
    void clear();

private:
    QWizardPage* introPage;
    QWizardPage* localPath;
    QWizardPage* conclusion;
    QStringList path;

    void createIntroPage();
    void createGetLocalPath();
    void createConclusion();

    //the following are fields that I cannot figure out how to access from other methods
    QLineEdit* pathDisplay;

private slots:
    void getPath();

};

#endif // NEWPROJECTWIZARD_H
