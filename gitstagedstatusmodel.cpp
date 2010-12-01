#include "gitstagedstatusmodel.h"
#include <QSettings>

GitStagedStatusModel::GitStagedStatusModel(QObject *parent) :
    QAbstractListModel(parent),
    fileList(new QStringList)
{

}
GitStagedStatusModel::~GitStagedStatusModel()
{
    delete fileList;
}
int GitStagedStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    return fileList->count();
}
QVariant GitStagedStatusModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();
    if (index.row() >= fileList->count() || index.row() < 0)
        return QVariant();
    if (role == Qt::DisplayRole)
    {
        return (*fileList)[index.row()];
    }
    return QVariant();
}
void GitStagedStatusModel::update(QStringList files)
{
    QRegExp rx("^(\\S.).*$"); // " M filname"  "MM filename" "AM filename"
    rx.setPatternSyntax(QRegExp::RegExp2);
    *fileList = files.filter(rx);
    emit dataChanged( createIndex(0,0), createIndex( fileList->count() ,0 ) );
}
