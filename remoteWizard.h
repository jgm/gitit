#ifndef REMOTEWIZARD_H
#define REMOTEWIZARD_H

#include <QWizard>
#include <QLineEdit>
#include <QPushButton>

class RemoteWizard : public QWizard
{
public:
    RemoteWizard();
    ~RemoteWizard();
    void clear();
    QString getPath();
    QString getName();

private:
    QWizardPage* introPage;
    QWizardPage* getInfo;
    QWizardPage* conclusion;

    void createIntroPage();
    void createInfoPage();
    void createConclusionPage();

    QLineEdit* path;
    QPushButton* browsePath;
    QLineEdit* remoteName;
};

#endif // REMOTEWIZARD_H
