function(key, values, rereduce)
{
  function my_reduce(values)
  {
    var infos = {}, comments = [];
    for(var v in values)
    {
      var value = values[v];
      if (value._id.info)
	infos[value._id.info] = value;
      else if (value._id.comments)
	comments.push(value);
    }
    var result = [];
    for(var c in comments)
    {
      var comment = comments[c];
      if (infos[comment._id.comments])
	infos[comment._id.comments].comments += comment.comments;
      else
	result.push(comment);
    }
    for(var i in infos)
      result.push(infos[i]);
    return result;
  }

  var r;
  if (rereduce)
  {
    r = [];
    for(var v in values)
      r += my_reduce(values[v]);
  }
  else
    r = my_reduce(values);

  r = r.sort(function(a, b)
	     {
	       if (a.date > b.date)
		 return -1;
	       else if (a.date < b.date)
		 return 1;
	       else
		 return 0;
	     });
  return r.slice(0, 100);
}