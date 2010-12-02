#ifndef NEWPROJECTWIZARD_H
#define NEWPROJECTWIZARD_H

#include<QWizard>
#include<QWizardPage>
#include<QLabel>
#include<QVBoxLayout>
#include<QButtonGroup>
#include<QRadioButton>
#include<QLineEdit>

class newProjectWizard : public QWizard
{
    Q_OBJECT

public:
    newProjectWizard();
};

#endif // NEWPROJECTWIZARD_H
