#include "remoteWizard.h"
#include <QLabel>
#include <QLayout>
#include "gitcommand.h"
RemoteWizard::RemoteWizard(GitCommand* gitCommand) :
        gitCommand(gitCommand)
{
    remoteName = new QLineEdit;
    path = new QLineEdit;
    browsePath = new QPushButton;
    createIntroPage();
    createInfoPage();
    createConclusionPage();

    this->addPage(introPage);
    this->addPage(getInfo);
    this->addPage(conclusion);
    connect(this,SIGNAL(accepted()),this,SLOT(addRemoteRepo()));
}

RemoteWizard::~RemoteWizard()
{
    delete introPage;
    delete getInfo;
    delete conclusion;
}

QString RemoteWizard::getName()
{

    return remoteName->text();
}

QString RemoteWizard::getPath()
{
    return path->text();
}

void RemoteWizard::createIntroPage()
{
    introPage = new QWizardPage;

    QLabel* label1 = new QLabel("I see that you would like to add a connection to a remote repository. You will need the location of that repository and the name.");
    label1->setWordWrap(true);
    QHBoxLayout* hLayout = new QHBoxLayout;

    hLayout->addWidget(label1);

    introPage->setLayout(hLayout);
}

void RemoteWizard::createInfoPage()
{
    getInfo = new QWizardPage;
    QLabel* label1 = new QLabel("Name the connection.");
    QLabel* label2 = new QLabel("Where is this repository located?");


    QVBoxLayout* vLayout = new QVBoxLayout;
    vLayout->addWidget(label1);
    vLayout->addWidget(remoteName);
    vLayout->addWidget(label2);
    vLayout->addWidget(path);

    getInfo->setLayout(vLayout);
}

void RemoteWizard::createConclusionPage()
{
    conclusion = new QWizardPage;

    QLabel* label1 = new QLabel("A connection was made for you for that remote repository. You now refer to that repository as how you named it.");
    label1->setWordWrap(true);
    QHBoxLayout* hLayout = new QHBoxLayout;

    hLayout->addWidget(label1);

    conclusion->setLayout(hLayout);
}

void RemoteWizard::clear()
{
    remoteName->clear();
    path->clear();
}
void RemoteWizard::addRemoteRepo()
{
    QStringList args;
    args << "remote" << "add" << getName() << getPath();
    gitCommand->run(args);
}
