function(key, values, rereduce)
{
  function my_reduce(values)
  {
    var infos = {}, more = [];
    for(var v in values)
    {
      var value = values[v];
      if (value._id.info)
	infos[value._id.info] = value;
      else
	more.push(value);
    }
    var result = [];
    for(var m in more)
    {
      var m1 = more[m];
      log({m1:m1});
      if (infos[m1._id.comments])
	infos[m1._id.comments].comments += m1.comments;
      else if (infos[m1._id.tracker])
      {
	infos[m1._id.tracker].downloaded += m1.downloaded;
	infos[m1._id.tracker].seeders += m1.seeders;
	infos[m1._id.tracker].leechers += m1.leechers;
      }
      else
	result.push(m1);
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
      r = r.sort(function(a, b)
		 {
		   if (a.date > b.date)
		     return -1;
		   else if (a.date < b.date)
		     return 1;
		   else
		     return 0;
		 });
      r = r.slice(0, 100);
  }
  else
    r = my_reduce(values);

  return r;
}