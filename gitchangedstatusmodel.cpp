#include "gitchangedstatusmodel.h"
#include <QSettings>

GitChangedStatusModel::GitChangedStatusModel(QObject *parent) :
    QAbstractListModel(parent),
    fileList(new QStringList)
{

}
GitChangedStatusModel::~GitChangedStatusModel()
{
    delete fileList;
}
int GitChangedStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    return fileList->count();
}
QVariant GitChangedStatusModel::data(const QModelIndex &index, int role) const
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
void GitChangedStatusModel::update(QStringList files)
{
    QRegExp rx("^(.\\S).*$"); // " M filname"  "MM filename" "AM filename"
    rx.setPatternSyntax(QRegExp::RegExp2);
    *fileList = files.filter(rx);
    emit dataChanged( createIndex(0,0), createIndex( fileList->count() ,0 ) );
}
