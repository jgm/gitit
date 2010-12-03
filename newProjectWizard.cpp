#include "newProjectWizard.h"

NewProjectWizard::NewProjectWizard()
{
   introPage = new QWizardPage;
   localPath = new QWizardPage;
   conclusion = new QWizardPage;

   //creating widgets that are data members because I can't figure out how to changed a widget from other pages
   pathDisplay = new QLineEdit("", localPath);

   createIntroPage();
   createGetLocalPath();
   createConclusion();

   this->addPage( introPage);
   this->addPage( localPath);
   this->addPage( conclusion);
}

NewProjectWizard::~NewProjectWizard()
{
    delete introPage;
    delete localPath;
    delete conclusion;
}

void NewProjectWizard::createIntroPage()
{
    QLabel* label1 = new QLabel("This is the wizard to help you create a new project. If you already have a project, see the existing project wizard.");
    label1->setWordWrap(true);

    QHBoxLayout* hLayout = new QHBoxLayout;
    hLayout->addWidget(label1);

    introPage->setLayout(hLayout);
}

void NewProjectWizard::createGetLocalPath()
{
    QLabel* label1 = new QLabel("Which directory would you like to use git for?\n"
                                "The best choice contains everything you need for your project.\n"
                                "\n");
    label1->setWordWrap(true);

//  QLineEdit* pathDisplay = new QLineEdit("", localPath);
    QPushButton* browse = new QPushButton("Browse", localPath);
    connect(browse, SIGNAL(clicked()), this, SLOT(getPath()));

    QHBoxLayout* hLayout = new QHBoxLayout;
    hLayout->addWidget(pathDisplay);
    hLayout->addWidget(browse);

    QVBoxLayout* vLayout = new QVBoxLayout;
    vLayout->addWidget(label1);
    vLayout->addLayout(hLayout);

    localPath->setLayout(vLayout);
}

void NewProjectWizard::createConclusion()
{
    QLabel* label1 = new QLabel("Your directory is now loaded into Gitit. You can now start your project. When you wish to make a version to save, open your directory in Gitit.");
    label1->setWordWrap(true);

    QHBoxLayout* hLayout = new QHBoxLayout;
    hLayout->addWidget(label1);

    conclusion->setLayout(hLayout);
}

void NewProjectWizard::clear()
{
    pathDisplay->clear();
}

void NewProjectWizard::getPath()
{
    QFileDialog dialog(this);
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setViewMode(QFileDialog::Detail);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dialog.exec())
        path = dialog.selectedFiles();

    pathDisplay->setText(path[0]);
}
