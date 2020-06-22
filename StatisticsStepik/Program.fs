open System
open FSharp.Data
open FSharp.Collections
open XPlot.Plotly
open MathNet.Numerics.Distributions


//Выборочное среднее
let mean (data:float List) =
     let result = data|> List.sum 
     result/float data.Length

//Выборочная дисперсия
let sd (data:float List) =
     let result = ref 0.
     let mean =mean data
     for x in data do
         result:=!result + (x-mean)**2.
     
     !result/ float ((List.length data))

//исправленная дисперсия s^2
let sd2 (data:float List) =
    let n = float data.Length
    n*(sd data)/(n-1.)  

//среднее квадратическое отклонение s
let sdeviation (data:float List)=
    sqrt(sd data)

//Выборочная медиана
let med (data:float List) =
         let n = data |> List.length
         let data1 = data |>List.sort
         if n%2=0 then
             let i=n/2
             (data1.[i]+data1.[i-1])/2.
         else data1.[(int n/2)]

//выборочная мода - это самый частотный элемент в выборке
let mode (data:float List) =
   let mutable result =  new System.Collections.Generic.Dictionary<float, float>()
   let (x,y) = ref 0., ref 0.
   let condition =fun x -> if result.ContainsKey x then result.[x]<-result.[x]+1.
                           else result.Add(x,1.)
   data |> List.iter (condition)
   
   for item in result do
        if item.Value > !y then x:=item.Key
                                y:=item.Value
        
   !x

//вычисление квартилей :Кваритили Q1=0.25,Q2=0.5, Q3=0.75
let quant (p:float) (data:float List) = 
        let sortlist = List.sort data
        let ind=int (p*float (List.length data))
        sortlist.[ind]

//выборочный коэффициент корреляции
let cor (data:(float*float) List)=
         let data1 = data |> List.map (fun (x,_)-> float x)
         let data2 = data |> List.map (fun (_,y)-> float y)
         let n = List.length data1
         let m1 = mean data1
         let m2 = mean data2
         let res1sd=sd data1 * float n
         let res2sd=sd data2 * float n
         let result = ref 0.
         for i in 0..n-1 do
             result:=!result + ((data1.[i]-m1)*(data2.[i]-m2))
         
         !result/sqrt(res1sd*res2sd)

//коэффициент вариации
let v (data:float List) =
    sdeviation data/mean data

//выборочный центральный момент r-го момента
let nu (r:float) (data:float List) =
    let result = ref 0.
    let mean = mean data
    let n = float data.Length
    for item in data do
        result:=!result + (item - mean)**r

    !result/n

//размах
let r (data:float List) =
    let sortlist = List.sort data
    List.last sortlist - sortlist.[0]

//средний межквартальный размах
let meanQ2Q1 (data:float List) =
        (quant 0.75 data - quant 0.25 data)/2.
//персентильный размах
let per_r (data:float List) =
          quant 90. data - quant 10. data

//коэффициент ассиметрии
let sk1 (data:float List)=
    nu 3. data/((sdeviation data)**3.)

//коэффициент аксцесса
let k (data:float List)=
    (nu 4. data)/((sd data)**2.) - 3.

//интерквантильный разброс
let iqr (data:float List) =
          quant 0.75 data - quant 0.25 data     

//квантильный коэффициент ассиметрии
let sk2 (data:float List)=
    (iqr data - 2.*(quant 0.75 data))
        /iqr data

//ящики с усами центральная линия это выборочная медиана top Q3 bot Q1, borderstop=Q3+1.5IQR bordersbot=Q1-1.5IQR

let showhist chart1 chart2 overlaidLayout =
    [chart1; chart2]
    |> Chart.Plot
    |> Chart.WithLayout overlaidLayout
    |> Chart.WithWidth 700
    |> Chart.WithHeight 500
    |> Chart.Show


//относительные частоты попадания элементов выборки в данные интервалы.
let eval data =
     let n =float (List.length data)
     let a=ref 0.
     let b=ref 0.
     let c=ref 0.
     let d=ref 0.
     let e=ref 0.
     let f=ref 0.
     let g=ref 0.
     let h=ref 0.
     for i in data do
         if i < 15. then printfn ":("
         elif i <= 15.75 then a := !a+1. 
         elif i <= 16.5 then b := !b+1.
         elif i <= 17.25 then c := !c+1.
         elif i <= 18.0 then d := !d+1.
         elif i <= 18.75 then e := !e+1.
         elif i <= 19.5 then f := !f+1.
         elif i <= 20.25 then g := !g+1.
         elif i <= 21.20 then h := !h+1.
     [!a/n;!b/n;!c/n;!d/n;!e/n;!f/n;!g/n;!h/n;]


[<EntryPoint>]
let main argv =
   //
   let value=[23.; 24.; 21.; 23.; 22.; 21.; 20.; 21.; 28.; 25.; 22.; 22.; 25.; 21.]|>List.sort
   
   let a=med value
   let resmode=mode value
   let resmean =mean value
   let ressd = sd value
   let resquant =quant 0.25 value
   let resquant1=quant 0.75 value
   let data=[(170., 66.); (182., 74.);(183., 77.);(180., 72.);(175., 67.);(181., 77.);(187., 76.);(181., 77.);(178., 72.);(187., 76.)]
   let rescor =cor data
   //
   //eval
   //let msft = CsvFile.Load("D:\F#\1.txt").Cache()
   //let somelist = [for row in msft.Headers.Value do yield float row]|>List.sort
   //
   let datacsv = CsvFile.Load("D:\F#\1.tsv",hasHeaders=false).Cache()
   let mutable datares = List.empty
   for row in datacsv.Rows do 
            let mutable column = List.empty
            for item in row.Columns do
                column <- column @ [item] 
            datares <- datares @ [column]
   let mutable sixnum = List.empty
   let mutable thirxnum = List.empty
   for item in datares do
        item |> List.iteri (fun i x -> if i=2 then sixnum<-sixnum @ [float x]
                                       elif i=3 then thirxnum<-thirxnum @[float x] )
  
   let n = sixnum.Length
   let mutable dif = List.Empty
   for i in 0..n-1 do
        dif <- dif @ [thirxnum.[i]-sixnum.[i]] 


   let chart1 =
       Histogram(
           x = sixnum,
           opacity = 0.75,
           name = "6-го числа"
       )
   
   let chart2 =
       Histogram(
           x = thirxnum,
           opacity = 0.75,
           name = "13 числа"
       )

   let chart3 =
       Histogram(
           x = dif,
           opacity = 0.75,
           name = "6-го числа"
       )
   let overlaidLayout =
       Layout(
           barmode = "overlay",
           title = "Overlaid Histogram"
       )
  
   let points =[|1. .. 50.|]
   Exponential.Samples(points,0.2) 

   let box1 = Box(y = sixnum,name="6-го числа",boxpoints = "all",
   jitter = 0.3,pointpos = -1.8)
   let box2 = Box(y = thirxnum,name = "13-го числа",boxpoints = "all",
   jitter = 0.3,pointpos = -1.8)

   let box3 = Box(y = dif,name = "111",boxpoints = "all",
   jitter = 0.3,pointpos = -1.8)
   
   [box3]
   |> Chart.Plot
   |> Chart.WithWidth 700
   |> Chart.WithHeight 500
   |> Chart.Show

   Console.ReadKey()
   0