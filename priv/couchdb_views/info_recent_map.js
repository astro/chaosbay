function(doc)
{
  if (doc._id.indexOf("info:") == 0)
  {
    var name = doc._id.substr(5);
    doc._id = {
      "info": name
    };
    doc.comments = 0;
    doc.downloaded = 0;
    doc.seeders = 0;
    doc.leechers = 0;
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
  else if (doc._id.indexOf("tracker:") == 0)
  {
    var name = doc.torrent;
    doc._id = {
      "tracker": name
    };
    doc.seeders = 0;
    doc.leechers = 0;
    for(var p in doc.peers)
    {
      var peer = doc.peers[p];
      if (peer.left == 0)
	doc.seeders++;
      else
	doc.leechers++;
    }
    doc.peers = null;
    emit([name, "tracker"], doc);
  }
}