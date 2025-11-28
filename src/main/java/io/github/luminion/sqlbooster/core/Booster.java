package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.SqlBuilderWrapper;
import io.github.luminion.sqlbooster.model.BoosterPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * VO查询能力接口.
 * <p>提供灵活的视图对象查询和类型转换功能.</p>
 *
 * <h3>方法命名规则：</h3>
 * <ul>
 *   <li>{@code voById} - 根据ID查询</li>
 *   <li>{@code voByIds} - 根据ID集合批量查询</li>
 *   <li>{@code voFirst} - 查询第一条（有多条返回第一条）</li>
 *   <li>{@code voUnique} - 查询唯一记录（多条抛异常）</li>
 *   <li>{@code voList} - 查询列表</li>
 *   <li>{@code voPage} - 查询列表(分页)</li>
 * </ul>
 *
 * @param <T> 数据库实体的类型。
 * @param <V> 要返回的视图对象 (VO) 的类型。
 * @author luminion
 * @since 1.0.0
 */
public interface Booster<T, V> {

    // ==================== 根据ID查询 ====================

    /**
     * 根据ID查询VO对象.
     *
     * @param id 主键ID
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    V voById(Serializable id);

    /**
     * 根据ID查询并转换为指定VO类型.
     *
     * @param id         主键ID
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    <R> R voById(Serializable id, Class<R> targetType);

    /**
     * 根据ID查询VO对象（返回Optional）.
     *
     * @param id 主键ID
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    Optional<V> voByIdOpt(Serializable id);

    /**
     * 根据ID查询并转换为指定VO类型（返回Optional）.
     *
     * @param id         主键ID
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    <R> Optional<R> voByIdOpt(Serializable id, Class<R> targetType);

    // ==================== 批量ID查询 ===================

    /**
     * 根据ID集合批量查询VO对象.
     *
     * @param ids ID集合
     * @return VO对象列表，无结果返回空列表
     * @since 1.0.0
     */
    List<V> voListByIds(Collection<? extends Serializable> ids);

    /**
     * 根据ID集合批量查询并转换为指定VO类型.
     *
     * @param ids        ID集合
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return VO对象列表，无结果返回空列表
     * @since 1.0.0
     */
    <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> targetType);

    // ==================== 查询第一条（voFirst） ====================

    /**
     * 根据条件查询第一个VO对象.
     * <p>如果查询结果有多条，返回第一条.</p>
     * <p>如果无结果，返回null.</p>
     *
     * @param sqlContext 查询条件
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    V voFirst(SqlContext<T> sqlContext);

    /**
     * 根据条件查询第一个VO对象并转换类型.
     *
     * @param sqlContext 查询条件
     * @param targetType   目标VO类型
     * @param <R>          目标VO类型
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    <R> R voFirst(SqlContext<T> sqlContext, Class<R> targetType);

    /**
     * 根据条件查询第一个VO对象（返回Optional）.
     *
     * @param sqlContext 查询条件
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    Optional<V> voFirstOpt(SqlContext<T> sqlContext);

    /**
     * 根据条件查询第一个VO对象并转换类型（返回Optional）.
     *
     * @param sqlContext 查询条件
     * @param targetType   目标VO类型
     * @param <R>          目标VO类型
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    <R> Optional<R> voFirstOpt(SqlContext<T> sqlContext, Class<R> targetType);

    // ==================== 查询唯一记录（voUnique） ====================

    /**
     * 根据条件查询唯一VO对象.
     * <p><b>注意：如果查询结果超过1条，将抛出异常</b></p>
     * <p>如果无结果，返回null.</p>
     *
     * @param sqlContext 查询条件
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    V voUnique(SqlContext<T> sqlContext);

    /**
     * 根据条件查询唯一VO对象并转换类型.
     * <p><b>注意：如果查询结果超过1条，将抛出异常</b></p>
     *
     * @param sqlContext 查询条件
     * @param targetType   目标VO类型
     * @param <R>          目标VO类型
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    <R> R voUnique(SqlContext<T> sqlContext, Class<R> targetType);

    /**
     * 根据条件查询唯一VO对象（返回Optional）.
     *
     * @param sqlContext 查询条件
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    Optional<V> voUniqueOpt(SqlContext<T> sqlContext);

    /**
     * 根据条件查询唯一VO对象并转换类型（返回Optional）.
     *
     * @param sqlContext 查询条件
     * @param targetType   目标VO类型
     * @param <R>          目标VO类型
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    <R> Optional<R> voUniqueOpt(SqlContext<T> sqlContext, Class<R> targetType);

    // ==================== 查询列表 ====================

    /**
     * 查询所有VO对象.
     *
     * @return VO对象列表，无结果返回空列表
     * @since 1.0.0
     */
    List<V> voList();

    /**
     * 根据条件查询VO对象列表.
     *
     * @param sqlContext 查询条件
     * @return VO对象列表，无结果返回空列表
     * @since 1.0.0
     */
    List<V> voList(SqlContext<T> sqlContext);

    /**
     * 根据条件查询VO对象列表并转换类型.
     *
     * @param sqlContext 查询条件
     * @param targetType   目标VO类型
     * @param <R>          目标VO类型
     * @return 转换后的VO对象列表
     * @since 1.0.0
     */
    <R> List<R> voList(SqlContext<T> sqlContext, Class<R> targetType);

    /**
     * 根据条件查询VO对象列表（分页）.
     *
     * @param sqlContext 查询条件
     * @param pageNum      页码
     * @param pageSize     每页数量
     * @return 分页结果对象
     * @since 1.0.0
     */
    BoosterPage<V> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize);


    /**
     * 根据条件查询VO对象列表（分页）.
     *
     * @param sqlContext 查询条件
     * @param pageNum      页码
     * @param pageSize     每页数量
     * @return 分页结果对象
     * @since 1.0.0
     */
    BoosterPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize);

    /**
     * 根据条件查询VO对象列表并转换类型（分页）.
     *
     * @param sqlContext 查询条件
     * @param pageNum      页码
     * @param pageSize     每页数量
     * @param targetType   目标VO类型
     * @param <R>          目标VO类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    <R> BoosterPage<R> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize, Class<R> targetType);

    /**
     * 根据条件查询VO对象列表（分页）.
     *
     * @param sqlContext 查询条件
     * @param pageNum      页码
     * @param pageSize     每页数量
     * @param targetType   目标VO类型
     * @param <R>          目标VO类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    <R> BoosterPage<R> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize, Class<R> targetType);

    /**
     * 获取SqlBuilderBooster对象，用于构建SQL查询条件.
     *
     * @return SqlBuilderBooster对象
     * @since 1.0.0
     */
    SqlBuilderWrapper<T, V> sqlBuilder();
}