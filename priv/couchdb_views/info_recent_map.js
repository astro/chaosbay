function(doc)
{
  if (doc._id.indexOf("info:") == 0)
  {
    var name = doc._id.substr(5);
    doc._id = {
      "info": name
    };
    doc.comments = 0;
    emit([name, "info"], doc);
  }
  else if (doc._id.indexOf("comments:") == 0)
  {
    var name = doc._id.substr(9);
    doc._id = {
      "comments": name
    };
    doc.comments = doc.comments.length;
    emit([name, "comment"], doc);
  }
}