package to_string

import (
	"testing"

	. "github.com/smartystreets/goconvey/convey"
)

func TestFmt(t *testing.T) {
	Convey("TestFmt", t, func() {
		type CommonStruct struct {
			IntSlice1    []int
			IntSlice2    []int
			IntSlice3    []int
			StructSlice1 []CommonStruct
			StructSlice2 []CommonStruct
			StructSlice3 []CommonStruct
			IntPtr       ***int
			Map          map[int]int
		}

		num := 1
		numP := &num
		numPP := &numP
		numPPP := &numPP
		cs := CommonStruct{
			IntPtr:    numPPP,
			IntSlice1: []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
			Map:       map[int]int{1: 2},
		}
		cs.IntSlice3 = cs.IntSlice1[2:5]
		cs.StructSlice1 = []CommonStruct{cs, cs, cs}
		cs.StructSlice2 = cs.StructSlice1[1:]
		cs.StructSlice3 = []CommonStruct{}

		Convey("test_no_type_info", func() {
			const loopCnt = 1000
			const expectStr = `{
			    IntSlice1:$Obj1(0-9),
			    IntSlice2:nil,
				IntSlice3:$Obj1(2-4),
				StructSlice1:$Obj2(0-2),
				StructSlice2:$Obj2(1-2),
				StructSlice3:[],
				IntPtr:1,
				Map:<Obj3>{
			            1:2
				}
			},
			{
				<Obj1>:[
					1(0),
					2,
					3(2),
					4,
					5(4),
					6,
					7,
					8,
					9,
					10(9)
				],
				<Obj2>:[
					{
						IntSlice1:$Obj1(0-9),
						IntSlice2:nil,
						IntSlice3:$Obj1(2-4),
						StructSlice1:nil,
						StructSlice2:nil,
						StructSlice3:nil,
						IntPtr:1,
						Map:$Obj3
					}(0),
					{
						IntSlice1:$Obj1(0-9),
						IntSlice2:nil,
						IntSlice3:$Obj1(2-4),
						StructSlice1:nil,
						StructSlice2:nil,
						StructSlice3:nil,
						IntPtr:1,
						Map:$Obj3
					}(1),
					{
						IntSlice1:$Obj1(0-9),
						IntSlice2:nil,
						IntSlice3:$Obj1(2-4),
						StructSlice1:nil,
						StructSlice2:nil,
						StructSlice3:nil,
						IntPtr:1,
						Map:$Obj3
					}(2)
				]
			}`

			for i := 1; i <= loopCnt; i++ {
				str, err := Fmt(StringByConf(cs, Config{
					InformationLevel: NoBaseKindsInfoOnly,
				}), 2)
				So(err, ShouldEqual, nil)
				So(str, ShouldEqual, expectStr)
			}
		})
	})
}
