package io.github.luminion.sqlbooster.core;

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
public interface BoosterCore<T, V> extends Booster<T, V> {

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
     * @param queryParam 查询条件
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    V voFirst(QueryParam<T> queryParam);

    /**
     * 根据条件查询第一个VO对象并转换类型.
     *
     * @param queryParam  查询条件
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    <R> R voFirst(QueryParam<T> queryParam, Class<R> targetType);

    /**
     * 根据条件查询第一个VO对象（返回Optional）.
     *
     * @param queryParam 查询条件
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    Optional<V> voFirstOpt(QueryParam<T> queryParam);

    /**
     * 根据条件查询第一个VO对象并转换类型（返回Optional）.
     *
     * @param queryParam  查询条件
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    <R> Optional<R> voFirstOpt(QueryParam<T> queryParam, Class<R> targetType);

    // ==================== 查询唯一记录（voUnique） ====================

    /**
     * 根据条件查询唯一VO对象.
     * <p><b>注意：如果查询结果超过1条，将抛出异常</b></p>
     * <p>如果无结果，返回null.</p>
     *
     * @param queryParam 查询条件
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    V voUnique(QueryParam<T> queryParam);

    /**
     * 根据条件查询唯一VO对象并转换类型.
     * <p><b>注意：如果查询结果超过1条，将抛出异常</b></p>
     *
     * @param queryParam  查询条件
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return VO对象，不存在返回null
     * @since 1.0.0
     */
    <R> R voUnique(QueryParam<T> queryParam, Class<R> targetType);

    /**
     * 根据条件查询唯一VO对象（返回Optional）.
     *
     * @param queryParam 查询条件
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    Optional<V> voUniqueOpt(QueryParam<T> queryParam);

    /**
     * 根据条件查询唯一VO对象并转换类型（返回Optional）.
     *
     * @param queryParam  查询条件
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return Optional包装的VO对象
     * @since 1.0.0
     */
    <R> Optional<R> voUniqueOpt(QueryParam<T> queryParam, Class<R> targetType);

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
     * @param queryParam 查询条件
     * @return VO对象列表，无结果返回空列表
     * @since 1.0.0
     */
    List<V> voList(QueryParam<T> queryParam);

    /**
     * 根据条件查询VO对象列表并转换类型.
     *
     * @param queryParam  查询条件
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return 转换后的VO对象列表
     * @since 1.0.0
     */
    <R> List<R> voList(QueryParam<T> queryParam, Class<R> targetType);

    /**
     * 根据条件查询VO对象列表（分页）.
     *
     * @param queryParam 查询条件
     * @param pageNum   页码
     * @param pageSize  每页数量
     * @return 分页结果对象
     * @since 1.0.0
     */
    BoosterPage<V> voPage(QueryParam<T> queryParam, int pageNum, int pageSize);


    /**
     * 根据条件查询VO对象列表（分页）.
     *
     * @param queryParam 查询条件
     * @param pageNum   页码
     * @param pageSize  每页数量
     * @return 分页结果对象
     * @since 1.0.0
     */
    BoosterPage<V> voPage(QueryParam<T> queryParam, long pageNum, long pageSize);

    /**
     * 根据条件查询VO对象列表并转换类型（分页）.
     *
     * @param queryParam  查询条件
     * @param pageNum    页码
     * @param pageSize   每页数量
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    <R> BoosterPage<R> voPage(QueryParam<T> queryParam, int pageNum, int pageSize, Class<R> targetType);

    /**
     * 根据条件查询VO对象列表（分页）.
     *
     * @param queryParam  查询条件
     * @param pageNum    页码
     * @param pageSize   每页数量
     * @param targetType 目标VO类型
     * @param <R>        目标VO类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    <R> BoosterPage<R> voPage(QueryParam<T> queryParam, long pageNum, long pageSize, Class<R> targetType);
}