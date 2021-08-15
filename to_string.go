package to_string

import (
	"bytes"
	"fmt"
	"reflect"
	"runtime/debug"
	"sort"
	"strconv"
)

// ts包用于生成指定golang对象对应的字符串, 该字符串以易于阅读的方式呈现指定对象的内容.
// ts包中相关函数的实现基于一种明确定义的有向图, 该有向图中的结点和边如下:
// 对象obj为有向图的源点, 任意结点的后继结点如下:
//     (1) 如果v.Kind()为reflect.Slice, 则切片v中包含的所有元素为v的后继结点
//     (2) 如果v.Kind()为reflect.Interface, 则v.Elem()为v的后继结点
//     (3) 如果v.Kind()为reflect.Array, 则v中包含的所有元素为v的后继结点
//     (4) 如果v.Kind()为reflect.Struct, 则v的所有字段对应的值为v的后继结点
//     (5) 如果v.Kind()为reflect.Map, 则v中所有Key, Value为v的后继结点
// 对于任意结点v, 当且仅当v满足如下条件时, v不存在后继结点:
// 	   (1) v.Kind()与后面给出的某个值相等: reflect.Invalid, reflect.String, reflect.Bool, reflect.Int, reflect.Int8
//	                                   , reflect.Int16, reflect.Int32, reflect.Int64, reflect.Uint, reflect.Uint8
//	                                   , reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr
//	                                   , reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128
//	                                   , reflect.Chan, reflect.Func, reflect.UnsafePointer
//     (2) v.Kind()为reflect.Interface且v.IsNil()为true
//     (3) v.Kind()为reflect.Array且v.Len()为0
//	   (4) v.Kind()为reflect.Slice且v.IsNil()为true或v.Len()为0
//	   (5) v.Kind()为reflect.Struct且cur.NumField()为0
//	   (6) v.Kind()的值为reflect.Map且v.IsNil()为true或v.Len()为0
//	   (7) v.Kind()的值为reflect.Ptr且v.IsNil()为true
// 所有指向指针的指针类型结点均视为两两不同的结点
// 将[]T类型的切片中直接包含的元素视为*T, 也即指向T类型的指针, 该指针指向的对象即为切片真实包含的元素.
// 如果两个对象满足如下条件之一, 则这两个对象在有向图中对应同一个结点:
//     (1) 为同类型的指针, 值不为nil且存储的地址值相等
//     (2) 为同类型的map, 值不为nil, map不为空且两个对象(map类型的变量实际对应一个指针值)的值相等
// 为使生成字符串的长度尽量少, 同时保证的生成字符串的直观性, 在遍历过程中遇到的重复的指针类型和map类型的结点时第1次遍历时完整
// 的呈现对象, 同时给对象编号, 编号形如Obj1, Obj2..., 之后再遇到该对象时使用占位符替换, 占位符形如$Obj1, $Obj2...,
// 	  使用ts.String不会展示对象的类型信息, 如果需要展示对象的类型信息也可以使用ts.StringByConf, 通过传入的conf进行控制
// 例如: 下面的程序:
// package main
//
// import (
//	 "fmt"
//
// )
//
// func main() {
//	 type Struct struct {
//		 str string
//	 }
//	 s := Struct{
//		 str: "str",
//	 }
//	 arr2 := []*Struct{&s, &s}
//	 num := 1
//	 arr1 := []*int{&num, &num}
//	 fmt.Printf("arr2:%v\n", ts.String(arr2))
//	 fmt.Printf("arr1:%v\n", ts.String(arr1))
//	 arr2Str := ts.StringByConf(arr2, ts.Config{
//		 InformationLevel: ts.AllTypesInfo,
//	 })
//	 fmt.Printf("arr2Str:%v\n", arr2Str)
//	 arr1Str := ts.StringByConf(arr1, ts.Config{
//		 InformationLevel: ts.AllTypesInfo,
//	 })
//	 fmt.Printf("arr1Str:%v\n", arr1Str)
// }
// 输出内容如下:
// arr2:[<Obj1>{str:"str"}, $Obj1]
// arr1:[1, 1]
// arr2Str:([]*main.Struct)[(*<Obj1>main.Struct){str:(string)"str"}, $Obj1]
// arr1Str:([]*int)[(*int)1, (*int)1]
//
// 为了进一步降低生成字符串的长度, 对于同类型的多个切片, 如果这些切片引用的底层数组之间存在共用的部分将对切片中的元素进行聚合, 保证该类型切片中
// 统一地址上的对象仅被打印一遍, 例如: 对于下面的程序:
// package main
//
// import (
//	 "fmt"
//
// )
//
// func main() {
//	 type Struct struct {
//		 slice1 []int
//		 slice2 []int
//	 }
//	 slice := []int{1, 2, 3, 4, 5}
//	 s := Struct{
//		 slice1: slice,
//		 slice2: slice[1:3],
//	 }
//	 fmt.Printf("s:%v\n", ts.String(s))
// }
// 输出内容如下:
// {slice1:$Obj1(0-4), slice2:$Obj1(1-2)}, {<Obj1>:[1(0), 2(1), 3(2), 4, 5(4)]}
//
// 上面的输出中slice1对应$Obj1(0-4)表示slice1中元素依次为Obj1的下标0至4之间的元素, slice2对应$Obj1(1-2)表示slice2中的元素依
// 次为Obj1的下标1至2之间的元素, 切片Obj1中的每个元素后面括号中的数字表示元素在Obj1中的下标
//
// 最后, 如果生成的字符串过大导致不便于阅读时可以使用本包下的Fmt函数格式化一下, 格式化之后层次分明, 阅读方便.

// 返回表示obj引用对象的字符串
func String(obj interface{}) string {
	return StringByConf(obj, Config{})
}

func StringByConf(obj interface{}, conf Config) (ret string) {
	defer func() {
		if err := recover(); err != nil {
			ret = fmt.Sprintf("Fatal error: panic in StringByConf, [Err]=%v\n[Stack]=\n%s\n", err, debug.Stack())
		}
	}()

	if conf.InformationLevel < NoTypesInfo || conf.InformationLevel > AllTypesInfo {
		panic(fmt.Errorf("error: config conf.InformationLevel illegal"))
	}
	for _, f := range conf.FilterStructField {
		if f == nil {
			panic(fmt.Errorf("error: config FilterStructField has nil func"))
		}
	}

	g := &graph{
		visited:                make(map[object]uint64),
		mapKeys:                make(map[object][]reflect.Value),
		inDegreeMoreThan1Nodes: make(map[object]struct{}),
		sliceElemInfo:          make(map[reflect.Type]map[uintptr]sliceElem),
		slices:                 make(map[reflect.Type][]slice),
		objStrs:                make(map[reflect.Value]string),
		conf:                   conf,
	}

	g.preProcessPhase = true
	g.dfs(reflect.ValueOf(obj), nil)

	for key, val := range g.visited {
		if val >= 2 {
			g.inDegreeMoreThan1Nodes[key] = struct{}{}
		}
	}
	g.visited = make(map[object]uint64)

	// 计算g.slices, 在第一次遍历到时写入Sn
	g.genSlices()

	g.preProcessPhase = false
	g.dfs(reflect.ValueOf(obj), nil)

	g.showShareSlices()

	return g.buf.String()
}

type InformationLevel int32

const (
	// 不展示类型信息
	NoTypesInfo InformationLevel = iota
	// 展示除基本类型之外的类型信息
	NoBaseKindsInfoOnly
	// 展示所有的类型信息
	AllTypesInfo
)

type Config struct {
	InformationLevel InformationLevel
	// 返回true表示过滤掉field对应的结点, 也即不在对field对应的结点递归访问
	FilterStructField []func(obj reflect.Value, fieldIdx int) (hitFilter bool)
	// ToString(obj)返回的objStr不为nil时使用*objStr作为obj的字符串呈现结果，为nil时则使用默认呈现规则
	ToString          func(obj reflect.Value) (objStr *string)
	DisableMapKeySort bool
}

type graph struct {
	// config中指定reflect.Value->对应的呈现结果
	objStrs map[reflect.Value]string

	preProcessPhase bool

	// key: 已经访问过的结点, value: key结点对应的编号
	visited map[object]uint64

	// 缓存在预处理阶段对map获取的key的序列, key: map对应的object, value: 对应的key序列.
	// 目的是是使预处理阶段和正式生成字符串阶段对同一个Map使用相同的key序列进行递归访问
	mapKeys map[object][]reflect.Value

	// 所有入度大于1的结点的集合
	inDegreeMoreThan1Nodes map[object]struct{}

	// 当前已经分配的最大结点序号
	curSn uint64

	// 存储所有切片元素的类型, 该序列的顺序即为遍历sliceElemInfo和slices使用key的顺序, 目的是保证输出结果的稳定性
	sliceElemTypes []reflect.Type

	// sliceElemInfo[t][p].Cnt表示[]t类型的切片中起始地址为p的t类型元素被引用的次数
	// sliceElemInfo[t][p].PtrValue表示[]t类型的切片中起始地址为p的t类型元素对应的指针对象
	sliceElemInfo map[reflect.Type]map[uintptr]sliceElem

	// slices[t][i]为类型t聚合之后的第i个切片, 切片中的元素为t类型元素对应的指针对象
	// slices[t][i].S对应[]t类型的切片的底层数组的起始地址小于slice[t][i+1].S对应切片的底层数组的起始地址
	slices map[reflect.Type][]slice

	buf bytes.Buffer

	conf Config
}

func (g *graph) dfs(node reflect.Value, preNode *reflect.Value) {
	switch node.Kind() {
	case reflect.Map:
		if g.preProcessPhase {
			if !g.handleCurObjForPreProcessPhase(node) {
				return
			}

			keys := node.MapKeys()

			if !g.conf.DisableMapKeySort {
				sort.Slice(keys, genCompareMapKeyFunc(keys))
			}

			g.mapKeys[toObj(node)] = keys

			for _, key := range keys {
				g.handleSuccObjForPreProcessPhase(key, false)
				g.handleSuccObjForPreProcessPhase(node.MapIndex(key), false)
			}

			return
		}

		if !g.handleCurObjForFormalPhase(node, preNode) {
			return
		}

		g.buf.WriteString("{")
		for i, key := range g.mapKeys[toObj(node)] {
			if i > 0 {
				g.buf.WriteString(", ")
			}
			g.dfs(key, &node)
			g.buf.WriteString(":")
			g.dfs(node.MapIndex(key), &node)
		}
		g.buf.WriteString("}")

		return
	case reflect.Array:
		if g.preProcessPhase {
			if !g.handleCurObjForPreProcessPhase(node) {
				return
			}

			for i := 0; i < node.Len(); i++ {
				g.handleSuccObjForPreProcessPhase(node.Index(i), false)
			}
			return
		}

		if !g.handleCurObjForFormalPhase(node, preNode) {
			return
		}

		g.buf.WriteString("[")
		for i := 0; i < node.Len(); i++ {
			if i > 0 {
				g.buf.WriteString(", ")
			}
			g.dfs(node.Index(i), &node)
		}
		g.buf.WriteString("]")

		return
	case reflect.Slice:
		if g.preProcessPhase {
			if !g.handleCurObjForPreProcessPhase(node) {
				return
			}
			for i := 0; i < node.Len(); i++ {
				g.handleSuccObjForPreProcessPhase(node.Index(i), true)
			}
			return
		}

		if !g.handleCurObjForFormalPhase(node, preNode) {
			return
		}

		g.buf.WriteString("[")
		for i := 0; i < node.Len(); i++ {
			if i > 0 {
				g.buf.WriteString(", ")
			}
			g.dfs(node.Index(i), &node)
		}
		g.buf.WriteString("]")

		return
	case reflect.Struct:
		if g.preProcessPhase {
			if !g.handleCurObjForPreProcessPhase(node) {
				return
			}
			for i := 0; i < node.NumField(); i++ {
				if g.filterStructField(node, i) {
					continue
				}

				g.handleSuccObjForPreProcessPhase(node.Field(i), false)
			}
			return
		}

		if !g.handleCurObjForFormalPhase(node, preNode) {
			return
		}

		g.buf.WriteString("{")
		var realWriteFieldCnt int
		for i := 0; i < node.NumField(); i++ {
			if g.filterStructField(node, i) {
				continue
			}

			if i > 0 && realWriteFieldCnt >= 1 {
				g.buf.WriteString(", ")
			}
			g.buf.WriteString(node.Type().Field(i).Name)
			g.buf.WriteString(":")
			g.dfs(node.Field(i), &node)
			realWriteFieldCnt++
		}
		g.buf.WriteString("}")

		return
	default:
		if g.preProcessPhase {
			if !g.handleCurObjForPreProcessPhase(node) {
				return
			}
			g.handleSuccObjForPreProcessPhase(node.Elem(), false)
			return
		}

		if !g.handleCurObjForFormalPhase(node, preNode) {
			return
		}
		g.dfs(node.Elem(), &node)
		return
	}
}

// 预处理阶段对当前结点cur的处理, 仅在cur存在后继结点时返回true
// cur.Kind()的合法值为所有reflect.Kind
func (g *graph) handleCurObjForPreProcessPhase(cur reflect.Value) bool {
	if cur.Kind() != reflect.Invalid && cur.CanInterface() && g.conf.ToString != nil {
		if objStr := g.conf.ToString(cur); objStr != nil {
			g.objStrs[cur] = *objStr
			return false
		}
	}

	switch cur.Kind() {
	case reflect.Interface:
		return !cur.IsNil()
	case reflect.Array:
		return cur.Len() > 0
	case reflect.Slice:
		return !cur.IsNil() && cur.Len() > 0
	case reflect.Struct:
		return cur.NumField() > 0
	case reflect.Map:
		if cur.IsNil() || cur.Len() == 0 {
			return false
		}
	case reflect.Ptr:
		if cur.IsNil() || isBaseKind(cur.Type().Elem().Kind()) {
			return false
		}

		if cur.Type().Elem().Kind() == reflect.Ptr {
			// 所有指向指针的指针类型结点均视为两两不同的结点
			return true
		}
	default:
		return false
	}

	g.visited[toObj(cur)]++
	return true
}

// 预处理阶段对当前结点的后继结点succ的处理，succ.Kind()的合法值为所有reflect.Kind
func (g *graph) handleSuccObjForPreProcessPhase(succ reflect.Value, isSliceElem bool) {
	if isSliceElem {
		if g.recordSliceElem(succ) {
			return
		}
		g.dfs(succ, nil)
		return
	}

	switch succ.Kind() {
	case reflect.Interface, reflect.Slice, reflect.Array, reflect.Struct:
		g.dfs(succ, nil)
		return
	case reflect.Ptr:
		if succ.IsNil() {
			return
		}
	case reflect.Map:
		if succ.IsNil() || succ.Len() == 0 {
			return
		}
	default:
		return
	}

	obj := toObj(succ)
	if _, inMap := g.visited[obj]; inMap {
		g.visited[obj]++
		return
	}
	g.dfs(succ, nil)
}

// 正式阶段对当前结点cur的处理, 仅在cur存在后继结点时返回true，cur.Kind()的合法值为所有reflect.Kind
func (g *graph) handleCurObjForFormalPhase(cur reflect.Value, preNode *reflect.Value) bool {
	func() {
		if g.conf.InformationLevel <= NoTypesInfo {
			return
		}

		switch cur.Kind() {
		case reflect.Ptr:
			if preNode == nil || preNode.Kind() != reflect.Ptr {
				g.buf.WriteByte('(')
			}

			if !cur.IsNil() {
				g.buf.WriteByte(ptrChar)
				return
			}

			// cur.IsNil() == true

			if preNode == nil || preNode.Kind() != reflect.Ptr {
				g.buf.WriteString(cur.Type().String())
			} else {
				g.buf.WriteByte(ptrChar)
				g.buf.WriteString(cur.Type().Elem().String())
			}

			g.buf.WriteByte(')')
		default:
			if preNode == nil || preNode.Kind() != reflect.Ptr {
				if isBaseKind(cur.Kind()) && g.conf.InformationLevel <= NoBaseKindsInfoOnly {
					return
				}

				if cur.Kind() == reflect.Invalid {
					return
				}

				g.buf.WriteByte('(')
				g.buf.WriteString(cur.Type().String())
				g.buf.WriteByte(')')
			} else if preNode.Kind() == reflect.Ptr {
				g.buf.WriteString(cur.Type().String())
				g.buf.WriteByte(')')
			}
		}
	}()

	if cur.Kind() != reflect.Ptr && preNode != nil && preNode.Kind() == reflect.Ptr {
		preObj := toObj(*preNode)
		if _, inMap := g.inDegreeMoreThan1Nodes[preObj]; inMap {
			if sn := g.visited[preObj]; sn == 0 {
				g.curSn++
				g.buf.WriteString(fmtObjSn(g.curSn))
				g.visited[preObj] = g.curSn
			} else {
				g.buf.WriteString(fmtPlaceholder(sn, nil, nil))
				return false
			}
		}
	}

	if cur.Kind() != reflect.Invalid {
		if objStr, inMap := g.objStrs[cur]; inMap {
			g.buf.WriteString(objStr)
			return false
		}
	}

	if isBaseKind(cur.Kind()) {
		switch cur.Kind() {
		case reflect.Invalid:
			g.buf.WriteString("nil")
		case reflect.String:
			g.buf.WriteString(strconv.Quote(cur.String()))
		case reflect.Bool:
			g.buf.WriteString(strconv.FormatBool(cur.Bool()))
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			g.buf.WriteString(strconv.FormatInt(cur.Int(), 10))
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
			g.buf.WriteString(strconv.FormatUint(cur.Uint(), 10))
		case reflect.Float32, reflect.Float64:
			g.buf.WriteString(strconv.FormatFloat(cur.Float(), 'f', -1, 64))
		case reflect.Complex64, reflect.Complex128:
			g.buf.WriteString(fmt.Sprintf("%v", cur.Complex()))
		default:
			g.buf.WriteString(fmt.Sprintf("0x%x", cur.Pointer()))
		}
		return false
	}

	switch cur.Kind() {
	case reflect.Interface:
		if cur.IsNil() {
			g.buf.WriteString("nil")
			return false
		}
		return true
	case reflect.Struct:
		if cur.NumField() == 0 {
			g.buf.WriteString("{}")
		}
		return cur.NumField() > 0
	case reflect.Array:
		if cur.Len() == 0 {
			g.buf.WriteString("[]")
		}
		return cur.Len() > 0
	case reflect.Slice:
		if cur.IsNil() {
			g.buf.WriteString("nil")
			return false
		}

		if cur.Len() == 0 {
			g.buf.WriteString("[]")
			return false
		}

		if g.slices[cur.Type().Elem()] == nil {
			return true
		}

		ty := cur.Type().Elem()
		ss := g.slices[ty]
		// 底层数组的起始地址
		ptr := cur.Pointer()

		pos := sort.Search(len(ss), func(i int) bool {
			return ss[i].S[0].Pointer() > ptr
		})

		for {
			pos--

			if pos < 0 {
				// 发生严重错误(可能是Obj被别的Goroutine并发修改导致整个对象图改变)
				g.buf.WriteString(fmt.Sprintf("<严重错误：无法定位元素所在切片，可能是Obj被别的Goroutine并发修改导致，Type:%s, Name:%s, Path:%s>", cur.Type().String(), cur.Type().Name(), cur.Type().PkgPath()))
				return false
			}

			p := ss[pos].S[0].Pointer()
			if p > ptr {
				continue
			}

			if ((ptr-p)/ty.Size())*ty.Size() == ptr-p {
				break
			}
		}

		// 计算在子切片中的下标
		beginIdx := int((ptr - ss[pos].S[0].Pointer()) / ty.Size())
		endIdx := beginIdx + cur.Len() - 1
		if endIdx > len(ss[pos].S)-1 {
			g.buf.WriteString(fmt.Sprintf("<严重错误：无法定位元素所在切片，可能是Obj被别的Goroutine并发修改导致，Type:%s, Name:%s, Path:%s>", cur.Type().String(), cur.Type().Name(), cur.Type().PkgPath()))
			return false
		}

		s := ss[pos]

		if s.Endpoints == nil {
			s.Endpoints = make(map[int]struct{})
		}
		s.Endpoints[beginIdx] = struct{}{}
		s.Endpoints[endIdx] = struct{}{}

		ss[pos] = s

		g.buf.WriteString(fmtPlaceholder(ss[pos].Sn, &beginIdx, &endIdx))

		return false
	case reflect.Ptr:
		if cur.IsNil() {
			g.buf.WriteString("nil")
			return false
		}

		if cur.Type().Elem().Kind() != reflect.Ptr {
			if _, inMap := g.inDegreeMoreThan1Nodes[toObj(cur)]; inMap {
				// 如果cur结点是指针类型，cur结点指向类型不是指针且cur结点的入度大于1，则cur结点信息的打印需要在对cur的后继结点的打印的调用的起始处进行
				return true
			}
		}
	case reflect.Map:
		if cur.IsNil() {
			g.buf.WriteString("nil")
			return false
		}

		if cur.Len() == 0 {
			g.buf.WriteString("{}")
			return false
		}
	default:
		panic(fmt.Errorf("unknown Kind, kind:%v", cur.Kind()))
	}

	obj := toObj(cur)

	if sn, inMap := g.visited[obj]; inMap {
		g.buf.WriteString(fmtPlaceholder(sn, nil, nil))
		return false
	}

	if _, inMap := g.inDegreeMoreThan1Nodes[obj]; inMap {
		g.curSn++
		g.buf.WriteString(fmtObjSn(g.curSn))
		g.visited[obj] = g.curSn
	}

	return true
}

func (g *graph) recordSliceElem(e reflect.Value) (visited bool) {
	ptrValue := e.Addr()
	ty := e.Type()
	ptr := ptrValue.Pointer()

	if g.sliceElemInfo[ty] == nil {
		g.sliceElemInfo[ty] = make(map[uintptr]sliceElem)
		g.sliceElemTypes = append(g.sliceElemTypes, ty)
	}

	info := g.sliceElemInfo[ty][ptr]
	info.Cnt++
	info.PtrValue = ptrValue
	g.sliceElemInfo[ty][ptr] = info

	return info.Cnt >= 2
}

func (g *graph) genSlices() {
	for _, ty := range g.sliceElemTypes {
		m := g.sliceElemInfo[ty]

		if len(m) == 0 {
			continue
		}

		var ptrValues []reflect.Value
		var isShareSlice bool

		for _, info := range m {
			ptrValues = append(ptrValues, info.PtrValue)
			if info.Cnt >= 2 {
				isShareSlice = true
			}
		}

		if !isShareSlice {
			// []ty类型的切片仅在一处被引用
			continue
		}

		sort.Slice(ptrValues, func(i, j int) bool {
			return ptrValues[i].Pointer() < ptrValues[j].Pointer()
		})

		var slices [][]reflect.Value
		remainElems := ptrValues

		// 对切片进行聚合
		for len(remainElems) > 0 {
			// 每次聚合生成切片的首元素均为remainElems[0]
			s := []reflect.Value{remainElems[0]}
			remainElemsCopy := remainElems
			remainElems = nil
			remainElemsCopy = remainElemsCopy[1:]

			for i, ptrValue := range remainElemsCopy {
				lastPtr := s[len(s)-1].Pointer()

				if ptrValue.Pointer() > lastPtr+ty.Size() {
					remainElems = append(remainElems, remainElemsCopy[i:]...)
					break
				}

				if ptrValue.Pointer() < lastPtr+ty.Size() {
					remainElems = append(remainElems, ptrValue)
					continue
				}

				s = append(s, ptrValue)
			}

			slices = append(slices, s)
		}

		g.slices[ty] = func() (ret []slice) {
			for _, v := range slices {
				g.curSn++
				ret = append(ret, slice{
					S:  v,
					Sn: g.curSn,
				})
			}
			return ret
		}()
	}
}

func (g *graph) showShareSlices() {
	if len(g.slices) == 0 {
		return
	}

	g.buf.WriteString(", ")
	g.buf.WriteByte('{')
	idx := 0

	for _, ty := range g.sliceElemTypes {
		for _, s := range g.slices[ty] {
			if idx >= 1 {
				g.buf.WriteString(", ")
			}

			g.buf.WriteString(fmtObjSn(s.Sn))
			g.buf.WriteByte(':')

			if g.conf.InformationLevel >= NoBaseKindsInfoOnly {
				g.buf.WriteString(fmt.Sprintf("([]%v)", s.S[0].Type().Elem()))
			}

			g.buf.WriteByte('[')

			for i := 0; i < len(s.S); i++ {
				if i > 0 {
					g.buf.WriteString(", ")
				}

				g.dfs(s.S[i].Elem(), nil)

				if _, inMap := s.Endpoints[i]; inMap {
					g.buf.WriteString(fmt.Sprintf("(%v)", i))
				}
			}

			g.buf.WriteByte(']')
			idx++
		}
	}
	g.buf.WriteByte('}')
}

// 返回true表示过滤掉field对应结点
func (g *graph) filterStructField(obj reflect.Value, fieldIdx int) (hitFilter bool) {
	for _, f := range g.conf.FilterStructField {
		if f(obj, fieldIdx) {
			return true
		}
	}
	return false
}

func toObj(n reflect.Value) object {
	return object{
		Type: n.Type(),
		Ptr:  n.Pointer(),
	}
}

func fmtObjSn(sn uint64) string {
	return fmt.Sprintf("<Obj%v>", sn)
}

func fmtPlaceholder(sn uint64, beginIdx, endIdx *int) string {
	if beginIdx == nil {
		return fmt.Sprintf("$Obj%v", sn)
	}
	return fmt.Sprintf("$Obj%v(%v-%v)", sn, *beginIdx, *endIdx)
}

const (
	ptrChar = '*'
)

type object struct {
	Type reflect.Type
	Ptr  uintptr
}

type sliceElem struct {
	Cnt      uint64
	PtrValue reflect.Value
}

type slice struct {
	Sn        uint64
	S         []reflect.Value
	Endpoints map[int]struct{}
}

func isBaseKind(v reflect.Kind) bool {
	switch v {
	case reflect.Bool, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Uintptr, reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128,
		reflect.Chan, reflect.Func, reflect.String, reflect.UnsafePointer, reflect.Invalid:
		return true
	}
	return false
}

func genCompareMapKeyFunc(keys []reflect.Value) func(int, int) bool {
	switch keys[0].Kind() {
	case reflect.String:
		return func(i, j int) bool {
			return keys[i].String() < keys[j].String()
		}
	case reflect.Bool:
		return func(i, j int) bool {
			return keys[i].Bool() && !keys[j].Bool()
		}
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return func(i, j int) bool {
			return keys[i].Int() < keys[j].Int()
		}
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32,
		reflect.Uint64, reflect.Uintptr:
		return func(i, j int) bool {
			return keys[i].Uint() < keys[j].Uint()
		}
	case reflect.UnsafePointer:
		return func(i, j int) bool {
			return keys[i].Pointer() < keys[j].Pointer()
		}
	case reflect.Float32, reflect.Float64:
		return func(i, j int) bool {
			return keys[i].Float() < keys[j].Float()
		}
	case reflect.Interface:
		return func(i, j int) bool {
			a, b := keys[i].InterfaceData(), keys[j].InterfaceData()
			if a[0] != b[0] {
				return a[0] < b[0]
			}
			return a[1] < b[1]
		}
	case reflect.Complex64, reflect.Complex128:
		return func(i, j int) bool {
			a, b := keys[i].Complex(), keys[j].Complex()
			if real(a) != real(b) {
				return real(a) < real(b)
			}
			return imag(a) < imag(b)
		}
	case reflect.Ptr, reflect.Chan:
		return func(i, j int) bool {
			a, b := keys[i].Pointer(), keys[j].Pointer()
			return a < b
		}
	case reflect.Struct:
		return func(i, j int) bool {
			a, b := keys[i], keys[j]
			for t := 0; t < a.NumField(); t++ {
				aField, bField := a.Field(t), b.Field(t)
				if genCompareMapKeyFunc([]reflect.Value{aField, bField})(0, 1) {
					return true
				}
			}
			return false
		}
	case reflect.Array:
		return func(i, j int) bool {
			a, b := keys[i], keys[j]
			for t := 0; t < a.Len(); t++ {
				aElem, bElem := a.Index(t), b.Index(t)
				if genCompareMapKeyFunc([]reflect.Value{aElem, bElem})(0, 1) {
					return true
				}
			}
			return false
		}
	default:
		panic(fmt.Errorf("unknown Kind, kind:%v", keys[0].Kind()))
	}
}
