open Spoc
open Kirc

let  vec_add = kern a b c n ->
  let open Std in
  let tid =  thread_idx_x + block_dim_x * block_idx_x in
  if tid < n then
    c.[<tid>] <-  a.[<tid>] +. b.[<tid>]

let size = 1024

let _ =
  Random.self_init ();
  let devs = Spoc.Devices.init () in
  let v1 = Vector.create Vector.float32 size
  and v2 = Vector.create Vector.float32 size
  and v3 = Vector.create Vector.float32 size
  in
  for i = 0 to Vector.length v1 -1 do
    v1.[<i>] <- Random.float 255.;
    v2.[<i>] <- Random.float 255.
  done;
  let threadsPerBlock = match devs.(0).Devices.specific_info with
    | Devices.OpenCLInfo clI ->
      (match clI.Devices.device_type with
       | Devices.CL_DEVICE_TYPE_CPU -> 1
       | _  ->   256)
    | _  -> 256 in
  let blocksPerGrid =
    (size + threadsPerBlock -1) / threadsPerBlock
  in
  let block0 = {Spoc.Kernel.blockX = threadsPerBlock;
                Spoc.Kernel.blockY = 1; Spoc.Kernel.blockZ = 1}
  and grid0= {Spoc.Kernel.gridX = blocksPerGrid;
              Spoc.Kernel.gridY = 1; Spoc.Kernel.gridZ = 1} in
  ignore(Kirc.gen vec_add);
  Kirc.run vec_add (v1,v2,v3,size) (block0,grid0) 0 devs.(0);
      
  for i = 0 to size - 1 do
    let tmp = v1.[<i>] +. v2.[<i>] -. v3.[<i>] in
    if tmp > 1.6e-5 || tmp < -1.6e-5 then
      (Printf.printf "Error : %g + %g <> %g -- diff = %g \n"  v1.[<i>] v2.[<i>] v3.[<i>] tmp)
  done;
  Printf.printf "Success\n%!"
