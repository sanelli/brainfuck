// <copyright file="BrainFuckException.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using System.Runtime.Serialization;

namespace Brainfuck;

/// <summary>
/// Brainfuck exception.
/// </summary>
[Serializable]
public sealed class BrainFuckException
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

    /// <summary>
    /// Initializes a new instance of the <see cref="BrainFuckException"/> class.
    /// </summary>
    /// <param name="info">The info.</param>
    /// <param name="context">The context.</param>
    private BrainFuckException(SerializationInfo info, StreamingContext context)
        : base(info, context)
    {
    }
}