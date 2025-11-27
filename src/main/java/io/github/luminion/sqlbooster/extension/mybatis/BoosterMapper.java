//package io.github.luminion.sqlbooster.extension.mybatis;
//
//import io.github.luminion.sqlbooster.core.BoosterEngine;
//import io.github.luminion.sqlbooster.core.BoosterPage;
//import io.github.luminion.sqlbooster.model.api.BoosterParam;
//import org.apache.ibatis.annotations.Param;
//
//import java.util.List;
//
///**
// * 一个通用的 Mapper 接口，用于执行返回视图对象 (VO) 的动态查询。
// * 该接口提供了一组用于常见查询操作的默认方法。
// *
// * @param <T> 数据库实体的类型。
// * @param <V> 要返回的视图对象 (VO) 的类型。
// * @author luminion
// * @since 1.0.0
// */
//public interface BoosterMapper<T, V> extends BoosterEngine<T, V> {
//
//    @Override
//    List<V> selectByBooster(@Param("wrapper") BoosterParam<T> wrapper, @Param("page") Object page);
//
//}
