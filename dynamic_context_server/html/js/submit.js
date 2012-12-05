
function subm(f,newtarget)
{
f.target = newtarget ;
f.submit();
}


function retarget_frame(w, exec, newtarget)
{
w.frames[newtarget].location.href = exec;
}
