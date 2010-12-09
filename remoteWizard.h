#ifndef REMOTEWIZARD_H
#define REMOTEWIZARD_H

#include <QWizard>
#include <QLineEdit>
#include <QPushButton>
class GitCommand;
class RemoteWizard : public QWizard
{
    Q_OBJECT
public:
    RemoteWizard(GitCommand* gitCommand);
    ~RemoteWizard();
    void clear();
    QString getPath();
    QString getName();

private:
    GitCommand* gitCommand;
    QWizardPage* introPage;
    QWizardPage* getInfo;
    QWizardPage* conclusion;

    void createIntroPage();
    void createInfoPage();
    void createConclusionPage();

    QLineEdit* path;
    QPushButton* browsePath;
    QLineEdit* remoteName;
public slots:
    void addRemoteRepo();
};

#endif // REMOTEWIZARD_H
