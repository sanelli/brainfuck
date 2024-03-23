// <copyright file="BrainFuckException.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using System.Runtime.Serialization;

namespace Brainfuck;

/// <summary>
/// Brainfuck exception.
/// </summary>
[Serializable]
 #pragma warning disable S3925
// SYSLIB0051 'Exception.Exception(SerializationInfo, StreamingContext)' is obsolete but SonarAnalyzer fails to realise this
public sealed class BrainFuckException
 #pragma warning restore S3925
    : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="BrainFuckException"/> class.
    /// </summary>
    /// <param name="message">The error message.</param>
    public BrainFuckException(string message)
        : base(message)
    {
    }
}