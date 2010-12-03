#include "existingProjectWizard.h"
#include <QFileDialog>
#include <QLineEdit>
#include <QPushButton>
#include <QComboBox>
#include "gitcommand.h"

/*
 TODO list
- implement ExistingProjectWizard::nextID() const
  - intro nextid is getLocalPath if local is picked
  - intro nextid is getRemotePath if remote is picked
- set required fields
 */

ExistingProjectWizard::ExistingProjectWizard() :
        gitCommand(new GitCommand),
        path()
{
    path << "";

    //creating widgets that are data members because I can't figure out how to changed a widget from other pages
    pathDisplay = new QLineEdit("", this);
    hiddenComment = new QLabel("");
    userRemotePath = new QLineEdit;
    userLocalDirectory = new QLineEdit("", this);
    localButton = new QRadioButton( "Local" );
    remoteButton = new QRadioButton("Remote");

    createIntroPage();
    createGetLocalPath();
    createGetRemotePath();
    createGetLocalDirectory();
    createEndNew();

    this->addPage( introPage);
    this->addPage( localPath);
    this->addPage( remotePath);
    this->addPage( localDirectory);
    this->addPage( conclusion);

    hiddenComment->setWordWrap(true);
    connect(this,SIGNAL(accepted()),this,SLOT(createRepo()));

}

ExistingProjectWizard::~ExistingProjectWizard()
{
    delete introPage;
    delete localPath;
    delete remotePath;
    delete localDirectory;
    delete conclusion;
    delete gitCommand;
}

int ExistingProjectWizard::nextId() const
{
    switch ( currentId() )
    {
    case 0: //introPage
        if ( localButton->isChecked() )
            return 1;
        else if( remoteButton->isChecked() )
            return 2;
        else
            return 0;
    case 1: //localPath
        return 4;
    case 2: //remotePath
        return 3;
    case 3: //localDirectory
        return 4;
    case 4: //conclusion
    default:
        return -1;
    }
}

void ExistingProjectWizard::createIntroPage()
{
    introPage = new QWizardPage;
    introPage->setTitle("Creating New Repository");

    QLabel* label1 = new QLabel("This is the wizard to help you use git for a new project. This is called creating a git repository for your project.");
    QLabel* label = new QLabel("Are you creating a git repository for a project on your local machine or in a remote location?");
    label1->setWordWrap(true);
    label->setWordWrap(true);

    QButtonGroup* projectLocationGroup = new QButtonGroup(introPage);
//  QRadioButton* localButton = new QRadioButton( "Local" );
//  QRadioButton* remoteButton = new QRadioButton("Remote");
    projectLocationGroup->addButton(localButton, 1);
    projectLocationGroup->addButton(remoteButton, 2);


    //creating the layout, adding widgets to it, and giving layout to the introPage
    QVBoxLayout* layout = new QVBoxLayout;
    layout->addWidget(label1);
    layout->addWidget(label);
    layout->addWidget(localButton);
    layout->addWidget(remoteButton);
    introPage->setLayout(layout);

}

void ExistingProjectWizard::createGetLocalPath()
{
    localPath = new QWizardPage;

    QLabel* label1 = new QLabel("Which directory would you like to use git for?\n"
                                "The best choice contains everything you need for your project.\n"
                                "\n");
    label1->setWordWrap(true);

//  QLineEdit* pathDisplay = new QLineEdit("", localPath);
    QPushButton* browse = new QPushButton("Browse", localPath);
    connect(browse, SIGNAL(clicked()), this, SLOT(getPath()));
    //localPath->registerField("pathDisplay*", pathDisplay);

    QHBoxLayout* hLayout = new QHBoxLayout;
    hLayout->addWidget(pathDisplay);
    hLayout->addWidget(browse);

    QVBoxLayout* vLayout = new QVBoxLayout;
    vLayout->addWidget(label1);
    vLayout->addLayout(hLayout);

    localPath->setLayout(vLayout);

}
void ExistingProjectWizard::createRepo()
{
    QStringList args;
    args << "init" << pathDisplay->text();
    gitCommand->run(args);

}

void ExistingProjectWizard::createGetRemotePath()
{
    remotePath = new QWizardPage;

    QLabel* label1 = new QLabel("Somewhere else is a directory that you would like to copy. That directory is already using git. This is called cloning a git repository.");
    label1->setWordWrap(true);

    QLabel* label2 = new QLabel("Where is the remote directory (that uses git) you would like to copy?");
    label2->setWordWrap(true);
    QComboBox* remoteOption = new QComboBox;
//  QLineEdit* userRemotePath = new QLineEdit;
    remoteOption->insertItem(0, "github");
    remoteOption->insertItem(1, "ssh");
    remoteOption->insertItem(2, "http");
    remoteOption->insertItem(3, "https");
    remoteOption->insertItem(4, "other");
    userRemotePath->setText("username/projectName");
    hiddenComment->setText( "This only works if you have write access to the Github project. This has to be explicitly set unless the project is your own."
                            "See github.com for questions and problems.");
    connect( remoteOption, SIGNAL(activated(int)), this, SLOT(displayHiddenComment(int)));

    //hiddenComment->setText( "This only works if you have write access to the Github project. This has to be explicitly set unless the project is your own.");

    QHBoxLayout* hLayout = new QHBoxLayout;
    hLayout->addWidget(remoteOption);
    hLayout->addWidget(userRemotePath);

//  QLabel* hiddenComment = new QLabel;

    QVBoxLayout* vLayout = new QVBoxLayout;
    vLayout->addWidget(label1);
    vLayout->addWidget(label2);
    vLayout->addLayout(hLayout);
    vLayout->addWidget(hiddenComment);
    remotePath->setLayout(vLayout);
}

void ExistingProjectWizard::createGetLocalDirectory()
{
    localDirectory = new QWizardPage;

    QLabel* label1 = new QLabel("Where would you like to store this directory that you are cloning?");

//  QLineEdit* userLocalDirectory = new QLineEdit("", localDirectory);
    QPushButton* browse = new QPushButton("Browse", localDirectory);
    connect(browse, SIGNAL(clicked()), this, SLOT(getPathToLocalDirectory()));

    QHBoxLayout* hLayout = new QHBoxLayout;
    hLayout->addWidget( userLocalDirectory);
    hLayout->addWidget( browse);


    QVBoxLayout* vLayout = new QVBoxLayout;
    vLayout->addWidget(label1);
    vLayout->addLayout(hLayout);

    localDirectory->setLayout(vLayout);
}

void ExistingProjectWizard::createEndNew()
{
    conclusion = new QWizardPage;

    QLabel* label1 = new QLabel("Your project now has a git directory. You will be able to use git (and this git gui) to version control your project."
                                "Upon exit of this wizard, you should be able to interact with your project.");
    label1->setWordWrap(true);

    QVBoxLayout* vLayout = new QVBoxLayout;
    vLayout->addWidget(label1);

    conclusion->setLayout(vLayout);
}

void ExistingProjectWizard::getPath()
{
    QFileDialog dialog(this);
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setViewMode(QFileDialog::Detail);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dialog.exec())
        path = dialog.selectedFiles();

    pathDisplay->setText(path[0]);
}
QString ExistingProjectWizard::getGitPath()
{
    return path[0];
}

void ExistingProjectWizard::getPathToLocalDirectory()
{
    QFileDialog dialog(this);
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setViewMode(QFileDialog::Detail);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dialog.exec())
        path = dialog.selectedFiles();

    userLocalDirectory->setText(path[0]);
}

void ExistingProjectWizard::displayHiddenComment(int index)
{
    if(index == 0)
    {
        userRemotePath->setText("username/projectName");
        hiddenComment->setText( "This only works if you have write access to the Github project. This has to be explicitly set unless the project is your own."
                                "See github.com for questions and problems.");
    }
    else if( index == 1)
    {
        hiddenComment->setText( "If you need to specify a different port, then the format is as follows: username@server.name:portNumber/path/to/directory");
        userRemotePath->setText("username@server.name/path/to/directory");
    }
    else if( index == 2)
    {
        hiddenComment->setText( "Enter in the url.");
        userRemotePath->setText( "http://urlname.com");
    }
    else if( index == 3)
    {
        hiddenComment->setText( "Enter in the url.");
        userRemotePath->setText( "https://urlname.com");
    }
    else if( index == 4)
    {
        hiddenComment->setText( "If you need to enter in another way to connect to the remote location, enter it here. Remember any prefixes need( git@, ssh://, etc).");
                userRemotePath->setText( "");
    }
}
